/*
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 *    Copyright (C) 2015 Tudor-Alin Miu
 *
 */
 
package uk.ac.ncl.onlineactivityrecognition.classification;

import java.io.File;
import java.io.FileReader;
import java.io.Reader;
import java.util.Random;

import weka.classifiers.bayes.NaiveBayesUpdateable;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;

public class OnlineBagNaiveBayes extends NaiveBayesUpdateable {

	private int numClassifiers;
	private NaiveBayesUpdateable[] classifiers;
	Random rand = new Random();
	
	private static final long serialVersionUID = 1L;

	public OnlineBagNaiveBayes(int numClassifiers) {
		super();
		this.numClassifiers = numClassifiers;
		classifiers = new NaiveBayesUpdateable[this.numClassifiers];
		for (int i = 0; i < classifiers.length; i++) {
			classifiers[i] = new NaiveBayesUpdateable();
		}		
	}

	@Override
	public void updateClassifier(Instance inst) throws Exception {
		int[] indices = generateClassifierIndices();
		for (int i : indices) {
			classifiers[i].updateClassifier(inst);
		}
	}

	@Override
	public double classifyInstance(Instance inst) throws Exception {
		int numClasses = classifiers[0].distributionForInstance(inst).length;
		int[] classCount = new int[numClasses]; 
		for (int i = 0; i < numClassifiers; i++) {
			int predictedClass = (int) classifiers[i].classifyInstance(inst);
			classCount[predictedClass]++;
		}

		int indexMax = 0;
		int max = classCount[indexMax];
		for (int i = 1; i < numClasses; i++) {
			if (classCount[i] > max) {
				max = classCount[i];
				indexMax = i;
			}
		}

		return indexMax;
	}

	@Override 
	public double[] distributionForInstance(Instance inst) throws Exception {
		int numClasses = classifiers[0].distributionForInstance(inst).length;
		double[] p = new double[numClasses];

		for (int i = 0; i < numClassifiers; i++) {
			double[] pIndividual = classifiers[i].distributionForInstance(inst);
			for (int k = 0; k < numClasses; k++) {
				p[k] += pIndividual[k] / (double) numClassifiers;
			}
		}
		return p;
	}

	@Override 
	public void buildClassifier(Instances insts) throws Exception {
		double[] weights = new double[insts.numInstances()];
		
		for (int i=0; i < numClassifiers; i++) {
			for (int k=0; k < weights.length; k++) {
				weights[k] = 1.0;
			}			
			
			Instances subInsts = insts.resampleWithWeights(rand, weights);
			classifiers[i].buildClassifier(subInsts);
		}
	}

	/*
	private int[] generateClassifierIndices() {
		int[] classifierIndices = new int[numActiveClassifiers];

		Random rand = new Random();
		int i = -1;
		while (i <= (numActiveClassifiers-2)) {
			int index = rand.nextInt(numActiveClassifiers);
			boolean found = false;
			for (int k = 0; k <= i; k++) {
				if (classifierIndices[k] == index) {
					found = true;
					break;
				}
			}
			if (!found) {
				classifierIndices[++i] = index;
			}
		}

		return classifierIndices;
	}
	*/
	
	private int[] generateClassifierIndices() {
		int[] classifierIndices = new int[numClassifiers];
		for (int i = 0; i < numClassifiers; i++) {
			classifierIndices[i] = rand.nextInt(numClassifiers);
		}
		
		return classifierIndices;
	}

	public static void main(String[] args) {
		try {
			String fileName = "C:\\Program Files\\Weka-3-7\\data\\iris.arff";
			Reader reader = new FileReader(new File(fileName));
			Instances dataset = new Instances(reader);
			dataset.setClassIndex(dataset.numAttributes() - 1);
			
			Instances initDataset = new Instances(dataset, 0, 1);
			System.out.println("Init instance class: " + initDataset.firstInstance().classValue());
			
			Instance newInst = dataset.instance(50);
			System.out.println("New instance class: " + newInst.classValue());
			
			Instance testInst = dataset.lastInstance();
			System.out.println("Test instance class: " + testInst.classValue());
			
			initDataset.setClassIndex(dataset.numAttributes()-1);

			int numClassifiers = 30;
			OnlineBagNaiveBayes bnb = new OnlineBagNaiveBayes(numClassifiers);

			bnb.buildClassifier(initDataset);
			
			bnb.updateClassifier(newInst);
			
			double[] predProb = bnb.distributionForInstance(testInst);
			double predClass = bnb.classifyInstance(testInst);
			
			for (int i = 0; i < predProb.length; i++) {
				System.out.print(predProb[i] + " ");
			}
			System.out.println("\npredicted class: " + predClass);
			
			System.out.println("Done!");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}

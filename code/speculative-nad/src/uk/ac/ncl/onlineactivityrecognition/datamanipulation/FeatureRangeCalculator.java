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
 
package uk.ac.ncl.onlineactivityrecognition.datamanipulation;

import java.io.Serializable;
import java.util.LinkedList;

import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics;

import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;

public class FeatureRangeCalculator implements Serializable {

	private static final long serialVersionUID = -433342982875405452L;

	private LinkedList<AccelerometerFrameFourDevices> unscaledFrames = new LinkedList<AccelerometerFrameFourDevices>();

	private double minPercentile = 0;
	private double maxPercentile = 1;

	private int DESCRIPTIVE_STATISTICS_WINDOW_LENGTH = 500;

	private boolean percentilesCalculated = false;

	// device one
	private double meanXDeviceOneMin = Double.POSITIVE_INFINITY;
	private double meanXDeviceOneMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics meanXDeviceOneStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double meanYDeviceOneMin = Double.POSITIVE_INFINITY;
	private double meanYDeviceOneMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics meanYDeviceOneStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double meanZDeviceOneMin = Double.POSITIVE_INFINITY;
	private double meanZDeviceOneMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics meanZDeviceOneStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);

	private double varXDeviceOneMin = Double.POSITIVE_INFINITY;
	private double varXDeviceOneMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics varXDeviceOneStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double varYDeviceOneMin = Double.POSITIVE_INFINITY;
	private double varYDeviceOneMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics varYDeviceOneStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double varZDeviceOneMin = Double.POSITIVE_INFINITY;
	private double varZDeviceOneMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics varZDeviceOneStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);

	private double corXYDeviceOneMin = Double.POSITIVE_INFINITY;
	private double corXYDeviceOneMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics corXYDeviceOneStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double corYZDeviceOneMin = Double.POSITIVE_INFINITY;
	private double corYZDeviceOneMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics corYZDeviceOneStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double corZXDeviceOneMin = Double.POSITIVE_INFINITY;
	private double corZXDeviceOneMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics corZXDeviceOneStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);

	// device two
	private double meanXDeviceTwoMin = Double.POSITIVE_INFINITY;
	private double meanXDeviceTwoMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics meanXDeviceTwoStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double meanYDeviceTwoMin = Double.POSITIVE_INFINITY;
	private double meanYDeviceTwoMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics meanYDeviceTwoStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double meanZDeviceTwoMin = Double.POSITIVE_INFINITY;
	private double meanZDeviceTwoMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics meanZDeviceTwoStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);

	private double varXDeviceTwoMin = Double.POSITIVE_INFINITY;
	private double varXDeviceTwoMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics varXDeviceTwoStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double varYDeviceTwoMin = Double.POSITIVE_INFINITY;
	private double varYDeviceTwoMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics varYDeviceTwoStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double varZDeviceTwoMin = Double.POSITIVE_INFINITY;
	private double varZDeviceTwoMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics varZDeviceTwoStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);

	private double corXYDeviceTwoMin = Double.POSITIVE_INFINITY;
	private double corXYDeviceTwoMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics corXYDeviceTwoStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double corYZDeviceTwoMin = Double.POSITIVE_INFINITY;
	private double corYZDeviceTwoMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics corYZDeviceTwoStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double corZXDeviceTwoMin = Double.POSITIVE_INFINITY;
	private double corZXDeviceTwoMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics corZXDeviceTwoStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);

	// device three
	private double meanXDeviceThreeMin = Double.POSITIVE_INFINITY;
	private double meanXDeviceThreeMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics meanXDeviceThreeStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double meanYDeviceThreeMin = Double.POSITIVE_INFINITY;
	private double meanYDeviceThreeMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics meanYDeviceThreeStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double meanZDeviceThreeMin = Double.POSITIVE_INFINITY;
	private double meanZDeviceThreeMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics meanZDeviceThreeStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);

	private double varXDeviceThreeMin = Double.POSITIVE_INFINITY;
	private double varXDeviceThreeMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics varXDeviceThreeStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double varYDeviceThreeMin = Double.POSITIVE_INFINITY;
	private double varYDeviceThreeMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics varYDeviceThreeStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double varZDeviceThreeMin = Double.POSITIVE_INFINITY;
	private double varZDeviceThreeMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics varZDeviceThreeStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);

	private double corXYDeviceThreeMin = Double.POSITIVE_INFINITY;
	private double corXYDeviceThreeMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics corXYDeviceThreeStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double corYZDeviceThreeMin = Double.POSITIVE_INFINITY;
	private double corYZDeviceThreeMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics corYZDeviceThreeStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double corZXDeviceThreeMin = Double.POSITIVE_INFINITY;
	private double corZXDeviceThreeMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics corZXDeviceThreeStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);

	// device four
	private double meanXDeviceFourMin = Double.POSITIVE_INFINITY;
	private double meanXDeviceFourMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics meanXDeviceFourStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double meanYDeviceFourMin = Double.POSITIVE_INFINITY;
	private double meanYDeviceFourMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics meanYDeviceFourStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double meanZDeviceFourMin = Double.POSITIVE_INFINITY;
	private double meanZDeviceFourMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics meanZDeviceFourStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);

	private double varXDeviceFourMin = Double.POSITIVE_INFINITY;
	private double varXDeviceFourMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics varXDeviceFourStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double varYDeviceFourMin = Double.POSITIVE_INFINITY;
	private double varYDeviceFourMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics varYDeviceFourStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double varZDeviceFourMin = Double.POSITIVE_INFINITY;
	private double varZDeviceFourMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics varZDeviceFourStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);

	private double corXYDeviceFourMin = Double.POSITIVE_INFINITY;
	private double corXYDeviceFourMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics corXYDeviceFourStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double corYZDeviceFourMin = Double.POSITIVE_INFINITY;
	private double corYZDeviceFourMax = Double.NEGATIVE_INFINITY;	
	private DescriptiveStatistics corYZDeviceFourStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);
	private double corZXDeviceFourMin = Double.POSITIVE_INFINITY;
	private double corZXDeviceFourMax = Double.NEGATIVE_INFINITY;
	private DescriptiveStatistics corZXDeviceFourStats = new DescriptiveStatistics(DESCRIPTIVE_STATISTICS_WINDOW_LENGTH);


	public FeatureRangeCalculator(double minPercentile, double maxPercentile) {
		this.minPercentile = minPercentile;
		this.maxPercentile = maxPercentile;
	}

	public void add(AccelerometerFrameFourDevices frame4d) 
			throws FrameNotFilledException {
		unscaledFrames.add(frame4d);

		// device one
		double meanXDeviceOne = frame4d.getMeanXDeviceOne();
		meanXDeviceOneStats.addValue(meanXDeviceOne);
		double meanYDeviceOne = frame4d.getMeanYDeviceOne();
		meanYDeviceOneStats.addValue(meanYDeviceOne);
		double meanZDeviceOne = frame4d.getMeanZDeviceOne();
		meanZDeviceOneStats.addValue(meanZDeviceOne);

		double varXDeviceOne = frame4d.getVarXDeviceOne();
		varXDeviceOneStats.addValue(varXDeviceOne);
		double varYDeviceOne = frame4d.getVarYDeviceOne();
		varYDeviceOneStats.addValue(varYDeviceOne);
		double varZDeviceOne = frame4d.getVarZDeviceOne();
		varZDeviceOneStats.addValue(varZDeviceOne);

		double corXYDeviceOne = frame4d.getCorXYDeviceOne();
		corXYDeviceOneStats.addValue(corXYDeviceOne);
		double corYZDeviceOne = frame4d.getCorYZDeviceOne();
		corYZDeviceOneStats.addValue(corYZDeviceOne);
		double corZXDeviceOne = frame4d.getCorZXDeviceOne();
		corZXDeviceOneStats.addValue(corZXDeviceOne);

		// device two
		double meanXDeviceTwo = frame4d.getMeanXDeviceTwo();
		meanXDeviceTwoStats.addValue(meanXDeviceTwo);
		double meanYDeviceTwo = frame4d.getMeanYDeviceTwo();
		meanYDeviceTwoStats.addValue(meanYDeviceTwo);
		double meanZDeviceTwo = frame4d.getMeanZDeviceTwo();
		meanZDeviceTwoStats.addValue(meanZDeviceTwo);

		double varXDeviceTwo = frame4d.getVarXDeviceTwo();
		varXDeviceTwoStats.addValue(varXDeviceTwo);
		double varYDeviceTwo = frame4d.getVarYDeviceTwo();
		varYDeviceTwoStats.addValue(varYDeviceTwo);
		double varZDeviceTwo = frame4d.getVarZDeviceTwo();
		varZDeviceTwoStats.addValue(varZDeviceTwo);

		double corXYDeviceTwo = frame4d.getCorXYDeviceTwo();
		corXYDeviceTwoStats.addValue(corXYDeviceTwo);
		double corYZDeviceTwo = frame4d.getCorYZDeviceTwo();
		corYZDeviceTwoStats.addValue(corYZDeviceTwo);
		double corZXDeviceTwo = frame4d.getCorZXDeviceTwo();
		corZXDeviceTwoStats.addValue(corZXDeviceTwo);

		// device three
		double meanXDeviceThree = frame4d.getMeanXDeviceThree();
		meanXDeviceThreeStats.addValue(meanXDeviceThree);
		double meanYDeviceThree = frame4d.getMeanYDeviceThree();
		meanYDeviceThreeStats.addValue(meanYDeviceThree);
		double meanZDeviceThree = frame4d.getMeanZDeviceThree();
		meanZDeviceThreeStats.addValue(meanZDeviceThree);

		double varXDeviceThree = frame4d.getVarXDeviceThree();
		varXDeviceThreeStats.addValue(varXDeviceThree);
		double varYDeviceThree = frame4d.getVarYDeviceThree();
		varYDeviceThreeStats.addValue(varYDeviceThree);
		double varZDeviceThree = frame4d.getVarZDeviceThree();
		varZDeviceThreeStats.addValue(varZDeviceThree);

		double corXYDeviceThree = frame4d.getCorXYDeviceThree();
		corXYDeviceThreeStats.addValue(corXYDeviceThree);
		double corYZDeviceThree = frame4d.getCorYZDeviceThree();
		corYZDeviceThreeStats.addValue(corYZDeviceThree);
		double corZXDeviceThree = frame4d.getCorZXDeviceThree();
		corZXDeviceThreeStats.addValue(corZXDeviceThree);

		// device four
		double meanXDeviceFour = frame4d.getMeanXDeviceFour();
		meanXDeviceFourStats.addValue(meanXDeviceFour);
		double meanYDeviceFour = frame4d.getMeanYDeviceFour();
		meanYDeviceFourStats.addValue(meanYDeviceFour);
		double meanZDeviceFour = frame4d.getMeanZDeviceFour();
		meanZDeviceFourStats.addValue(meanZDeviceFour);

		double varXDeviceFour = frame4d.getVarXDeviceFour();
		varXDeviceFourStats.addValue(varXDeviceFour);
		double varYDeviceFour = frame4d.getVarYDeviceFour();
		varYDeviceFourStats.addValue(varYDeviceFour);
		double varZDeviceFour = frame4d.getVarZDeviceFour();
		varZDeviceFourStats.addValue(varZDeviceFour);

		double corXYDeviceFour = frame4d.getCorXYDeviceFour();
		corXYDeviceFourStats.addValue(corXYDeviceFour);
		double corYZDeviceFour = frame4d.getCorYZDeviceFour();
		corYZDeviceFourStats.addValue(corYZDeviceFour);
		double corZXDeviceFour = frame4d.getCorZXDeviceFour();
		corZXDeviceFourStats.addValue(corZXDeviceFour);
	}

	public AccelerometerFrameFourDevices scaleFrame(AccelerometerFrameFourDevices frame4d) 
			throws FrameNotFilledException {
		long tStart = frame4d.getStartTimestamp();
		long tEnd = frame4d.getEndTimestamp();

		//device one
		AccelerometerFrame scaledFrameDeviceOne = new AccelerometerFrame(tStart, tEnd);

		double meanXDeviceOne = frame4d.getMeanXDeviceOne();
		scaledFrameDeviceOne.setMeanX(scale(meanXDeviceOneMin, meanXDeviceOne, meanXDeviceOneMax));		
		double meanYDeviceOne = frame4d.getMeanYDeviceOne();
		scaledFrameDeviceOne.setMeanY(scale(meanYDeviceOneMin, meanYDeviceOne, meanYDeviceOneMax));		
		double meanZDeviceOne = frame4d.getMeanZDeviceOne();
		scaledFrameDeviceOne.setMeanZ(scale(meanZDeviceOneMin, meanZDeviceOne, meanZDeviceOneMax));

		double varXDeviceOne = frame4d.getVarXDeviceOne();
		scaledFrameDeviceOne.setVarX(scale(varXDeviceOneMin, varXDeviceOne, varXDeviceOneMax));
		double varYDeviceOne = frame4d.getVarYDeviceOne();
		scaledFrameDeviceOne.setVarY(scale(varYDeviceOneMin, varYDeviceOne, varYDeviceOneMax));
		double varZDeviceOne = frame4d.getVarZDeviceOne();
		scaledFrameDeviceOne.setVarZ(scale(varZDeviceOneMin, varZDeviceOne, varZDeviceOneMax));

		double corXYDeviceOne = frame4d.getCorXYDeviceOne();
		scaledFrameDeviceOne.setCorXY(scale(corXYDeviceOneMin, corXYDeviceOne, corXYDeviceOneMax));
		double corYZDeviceOne = frame4d.getCorYZDeviceOne();
		scaledFrameDeviceOne.setCorYZ(scale(corYZDeviceOneMin, corYZDeviceOne, corYZDeviceOneMax));
		double corZXDeviceOne = frame4d.getCorZXDeviceOne();
		scaledFrameDeviceOne.setCorZX(scale(corZXDeviceOneMin, corZXDeviceOne, corZXDeviceOneMax));

		if (frame4d.getTDeviceOne() != null) {
			scaledFrameDeviceOne.setT(frame4d.getTDeviceOne());
		}
		scaledFrameDeviceOne.setFilled(false);

		//device two
		AccelerometerFrame scaledFrameDeviceTwo = new AccelerometerFrame(tStart, tEnd);

		double meanXDeviceTwo = frame4d.getMeanXDeviceTwo();
		scaledFrameDeviceTwo.setMeanX(scale(meanXDeviceTwoMin, meanXDeviceTwo, meanXDeviceTwoMax));		
		double meanYDeviceTwo = frame4d.getMeanYDeviceTwo();
		scaledFrameDeviceTwo.setMeanY(scale(meanYDeviceTwoMin, meanYDeviceTwo, meanYDeviceTwoMax));		
		double meanZDeviceTwo = frame4d.getMeanZDeviceTwo();
		scaledFrameDeviceTwo.setMeanZ(scale(meanZDeviceTwoMin, meanZDeviceTwo, meanZDeviceTwoMax));

		double varXDeviceTwo = frame4d.getVarXDeviceTwo();
		scaledFrameDeviceTwo.setVarX(scale(varXDeviceTwoMin, varXDeviceTwo, varXDeviceTwoMax));
		double varYDeviceTwo = frame4d.getVarYDeviceTwo();
		scaledFrameDeviceTwo.setVarY(scale(varYDeviceTwoMin, varYDeviceTwo, varYDeviceTwoMax));
		double varZDeviceTwo = frame4d.getVarZDeviceTwo();
		scaledFrameDeviceTwo.setVarZ(scale(varZDeviceTwoMin, varZDeviceTwo, varZDeviceTwoMax));

		double corXYDeviceTwo = frame4d.getCorXYDeviceTwo();
		scaledFrameDeviceTwo.setCorXY(scale(corXYDeviceTwoMin, corXYDeviceTwo, corXYDeviceTwoMax));
		double corYZDeviceTwo = frame4d.getCorYZDeviceTwo();
		scaledFrameDeviceTwo.setCorYZ(scale(corYZDeviceTwoMin, corYZDeviceTwo, corYZDeviceTwoMax));
		double corZXDeviceTwo = frame4d.getCorZXDeviceTwo();
		scaledFrameDeviceTwo.setCorZX(scale(corZXDeviceTwoMin, corZXDeviceTwo, corZXDeviceTwoMax));

		if (frame4d.getTDeviceTwo() != null) {
			scaledFrameDeviceTwo.setT(frame4d.getTDeviceTwo());
		}
		scaledFrameDeviceTwo.setFilled(false);

		//device three
		AccelerometerFrame scaledFrameDeviceThree = new AccelerometerFrame(tStart, tEnd);

		double meanXDeviceThree = frame4d.getMeanXDeviceThree();
		scaledFrameDeviceThree.setMeanX(scale(meanXDeviceThreeMin, meanXDeviceThree, meanXDeviceThreeMax));		
		double meanYDeviceThree = frame4d.getMeanYDeviceThree();
		scaledFrameDeviceThree.setMeanY(scale(meanYDeviceThreeMin, meanYDeviceThree, meanYDeviceThreeMax));		
		double meanZDeviceThree = frame4d.getMeanZDeviceThree();
		scaledFrameDeviceThree.setMeanZ(scale(meanZDeviceThreeMin, meanZDeviceThree, meanZDeviceThreeMax));

		double varXDeviceThree = frame4d.getVarXDeviceThree();
		scaledFrameDeviceThree.setVarX(scale(varXDeviceThreeMin, varXDeviceThree, varXDeviceThreeMax));
		double varYDeviceThree = frame4d.getVarYDeviceThree();
		scaledFrameDeviceThree.setVarY(scale(varYDeviceThreeMin, varYDeviceThree, varYDeviceThreeMax));
		double varZDeviceThree = frame4d.getVarZDeviceThree();
		scaledFrameDeviceThree.setVarZ(scale(varZDeviceThreeMin, varZDeviceThree, varZDeviceThreeMax));

		double corXYDeviceThree = frame4d.getCorXYDeviceThree();
		scaledFrameDeviceThree.setCorXY(scale(corXYDeviceThreeMin, corXYDeviceThree, corXYDeviceThreeMax));
		double corYZDeviceThree = frame4d.getCorYZDeviceThree();
		scaledFrameDeviceThree.setCorYZ(scale(corYZDeviceThreeMin, corYZDeviceThree, corYZDeviceThreeMax));
		double corZXDeviceThree = frame4d.getCorZXDeviceThree();
		scaledFrameDeviceThree.setCorZX(scale(corZXDeviceThreeMin, corZXDeviceThree, corZXDeviceThreeMax));

		if (frame4d.getTDeviceThree() != null) {
			scaledFrameDeviceThree.setT(frame4d.getTDeviceThree());
		}
		scaledFrameDeviceThree.setFilled(false);

		//device four
		AccelerometerFrame scaledFrameDeviceFour = new AccelerometerFrame(tStart, tEnd);

		double meanXDeviceFour = frame4d.getMeanXDeviceFour();
		scaledFrameDeviceFour.setMeanX(scale(meanXDeviceFourMin, meanXDeviceFour, meanXDeviceFourMax));		
		double meanYDeviceFour = frame4d.getMeanYDeviceFour();
		scaledFrameDeviceFour.setMeanY(scale(meanYDeviceFourMin, meanYDeviceFour, meanYDeviceFourMax));		
		double meanZDeviceFour = frame4d.getMeanZDeviceFour();
		scaledFrameDeviceFour.setMeanZ(scale(meanZDeviceFourMin, meanZDeviceFour, meanZDeviceFourMax));

		double varXDeviceFour = frame4d.getVarXDeviceFour();
		scaledFrameDeviceFour.setVarX(scale(varXDeviceFourMin, varXDeviceFour, varXDeviceFourMax));
		double varYDeviceFour = frame4d.getVarYDeviceFour();
		scaledFrameDeviceFour.setVarY(scale(varYDeviceFourMin, varYDeviceFour, varYDeviceFourMax));
		double varZDeviceFour = frame4d.getVarZDeviceFour();
		scaledFrameDeviceFour.setVarZ(scale(varZDeviceFourMin, varZDeviceFour, varZDeviceFourMax));

		double corXYDeviceFour = frame4d.getCorXYDeviceFour();
		scaledFrameDeviceFour.setCorXY(scale(corXYDeviceFourMin, corXYDeviceFour, corXYDeviceFourMax));
		double corYZDeviceFour = frame4d.getCorYZDeviceFour();
		scaledFrameDeviceFour.setCorYZ(scale(corYZDeviceFourMin, corYZDeviceFour, corYZDeviceFourMax));
		double corZXDeviceFour = frame4d.getCorZXDeviceFour();
		scaledFrameDeviceFour.setCorZX(scale(corZXDeviceFourMin, corZXDeviceFour, corZXDeviceFourMax));

		if (frame4d.getTDeviceFour() != null) {
			scaledFrameDeviceFour.setT(frame4d.getTDeviceFour());
		}
		scaledFrameDeviceFour.setFilled(false);

		return new AccelerometerFrameFourDevices(scaledFrameDeviceOne, scaledFrameDeviceTwo, 
				scaledFrameDeviceThree, scaledFrameDeviceFour);
		//return new AccelerometerFrame[] {scaledFrameDeviceOne, scaledFrameDeviceTwo, scaledFrameDeviceThree, 
		//		scaledFrameDeviceFour};
	}

	private double scale(double min, double actual, double max) {
		if (percentilesCalculated) {
			double v = (actual - min) / (max - min);
			if (Double.isNaN(v)) {
				return 0.5;
			} else {
				return v < 1 ? v : 1;
			} 
		} else {
			return 0.5;
		}
	}	

	public void calculateRanges() {
		// device one
		meanXDeviceOneMin = meanXDeviceOneStats.getPercentile(minPercentile);
		meanXDeviceOneMax = meanXDeviceOneStats.getPercentile(maxPercentile);
		meanYDeviceOneMin = meanYDeviceOneStats.getPercentile(minPercentile);
		meanYDeviceOneMax = meanYDeviceOneStats.getPercentile(maxPercentile);
		meanZDeviceOneMin = meanZDeviceOneStats.getPercentile(minPercentile);
		meanZDeviceOneMax = meanZDeviceOneStats.getPercentile(maxPercentile);

		varXDeviceOneMin = varXDeviceOneStats.getPercentile(minPercentile);
		varXDeviceOneMax = varXDeviceOneStats.getPercentile(maxPercentile);
		varYDeviceOneMin = varYDeviceOneStats.getPercentile(minPercentile);
		varYDeviceOneMax = varYDeviceOneStats.getPercentile(maxPercentile);
		varZDeviceOneMin = varZDeviceOneStats.getPercentile(minPercentile);
		varZDeviceOneMax = varZDeviceOneStats.getPercentile(maxPercentile);

		corXYDeviceOneMin = corXYDeviceOneStats.getPercentile(minPercentile);
		corXYDeviceOneMax = corXYDeviceOneStats.getPercentile(maxPercentile);
		corYZDeviceOneMin = corYZDeviceOneStats.getPercentile(minPercentile);
		corYZDeviceOneMax = corYZDeviceOneStats.getPercentile(maxPercentile);
		corZXDeviceOneMin = corZXDeviceOneStats.getPercentile(minPercentile);
		corZXDeviceOneMax = corZXDeviceOneStats.getPercentile(maxPercentile);

		// device two
		meanXDeviceTwoMin = meanXDeviceTwoStats.getPercentile(minPercentile);
		meanXDeviceTwoMax = meanXDeviceTwoStats.getPercentile(maxPercentile);
		meanYDeviceTwoMin = meanYDeviceTwoStats.getPercentile(minPercentile);
		meanYDeviceTwoMax = meanYDeviceTwoStats.getPercentile(maxPercentile);
		meanZDeviceTwoMin = meanZDeviceTwoStats.getPercentile(minPercentile);
		meanZDeviceTwoMax = meanZDeviceTwoStats.getPercentile(maxPercentile);

		varXDeviceTwoMin = varXDeviceTwoStats.getPercentile(minPercentile);
		varXDeviceTwoMax = varXDeviceTwoStats.getPercentile(maxPercentile);
		varYDeviceTwoMin = varYDeviceTwoStats.getPercentile(minPercentile);
		varYDeviceTwoMax = varYDeviceTwoStats.getPercentile(maxPercentile);
		varZDeviceTwoMin = varZDeviceTwoStats.getPercentile(minPercentile);
		varZDeviceTwoMax = varZDeviceTwoStats.getPercentile(maxPercentile);

		corXYDeviceTwoMin = corXYDeviceTwoStats.getPercentile(minPercentile);
		corXYDeviceTwoMax = corXYDeviceTwoStats.getPercentile(maxPercentile);
		corYZDeviceTwoMin = corYZDeviceTwoStats.getPercentile(minPercentile);
		corYZDeviceTwoMax = corYZDeviceTwoStats.getPercentile(maxPercentile);
		corZXDeviceTwoMin = corZXDeviceTwoStats.getPercentile(minPercentile);
		corZXDeviceTwoMax = corZXDeviceTwoStats.getPercentile(maxPercentile);

		// device three
		meanXDeviceThreeMin = meanXDeviceThreeStats.getPercentile(minPercentile);
		meanXDeviceThreeMax = meanXDeviceThreeStats.getPercentile(maxPercentile);
		meanYDeviceThreeMin = meanYDeviceThreeStats.getPercentile(minPercentile);
		meanYDeviceThreeMax = meanYDeviceThreeStats.getPercentile(maxPercentile);
		meanZDeviceThreeMin = meanZDeviceThreeStats.getPercentile(minPercentile);
		meanZDeviceThreeMax = meanZDeviceThreeStats.getPercentile(maxPercentile);

		varXDeviceThreeMin = varXDeviceThreeStats.getPercentile(minPercentile);
		varXDeviceThreeMax = varXDeviceThreeStats.getPercentile(maxPercentile);
		varYDeviceThreeMin = varYDeviceThreeStats.getPercentile(minPercentile);
		varYDeviceThreeMax = varYDeviceThreeStats.getPercentile(maxPercentile);
		varZDeviceThreeMin = varZDeviceThreeStats.getPercentile(minPercentile);
		varZDeviceThreeMax = varZDeviceThreeStats.getPercentile(maxPercentile);

		corXYDeviceThreeMin = corXYDeviceThreeStats.getPercentile(minPercentile);
		corXYDeviceThreeMax = corXYDeviceThreeStats.getPercentile(maxPercentile);
		corYZDeviceThreeMin = corYZDeviceThreeStats.getPercentile(minPercentile);
		corYZDeviceThreeMax = corYZDeviceThreeStats.getPercentile(maxPercentile);
		corZXDeviceThreeMin = corZXDeviceThreeStats.getPercentile(minPercentile);
		corZXDeviceThreeMax = corZXDeviceThreeStats.getPercentile(maxPercentile);

		// device four
		meanXDeviceFourMin = meanXDeviceFourStats.getPercentile(minPercentile);
		meanXDeviceFourMax = meanXDeviceFourStats.getPercentile(maxPercentile);
		meanYDeviceFourMin = meanYDeviceFourStats.getPercentile(minPercentile);
		meanYDeviceFourMax = meanYDeviceFourStats.getPercentile(maxPercentile);
		meanZDeviceFourMin = meanZDeviceFourStats.getPercentile(minPercentile);
		meanZDeviceFourMax = meanZDeviceFourStats.getPercentile(maxPercentile);

		varXDeviceFourMin = varXDeviceFourStats.getPercentile(minPercentile);
		varXDeviceFourMax = varXDeviceFourStats.getPercentile(maxPercentile);
		varYDeviceFourMin = varYDeviceFourStats.getPercentile(minPercentile);
		varYDeviceFourMax = varYDeviceFourStats.getPercentile(maxPercentile);
		varZDeviceFourMin = varZDeviceFourStats.getPercentile(minPercentile);
		varZDeviceFourMax = varZDeviceFourStats.getPercentile(maxPercentile);

		corXYDeviceFourMin = corXYDeviceFourStats.getPercentile(minPercentile);
		corXYDeviceFourMax = corXYDeviceFourStats.getPercentile(maxPercentile);
		corYZDeviceFourMin = corYZDeviceFourStats.getPercentile(minPercentile);
		corYZDeviceFourMax = corYZDeviceFourStats.getPercentile(maxPercentile);
		corZXDeviceFourMin = corZXDeviceFourStats.getPercentile(minPercentile);
		corZXDeviceFourMax = corZXDeviceFourStats.getPercentile(maxPercentile);

		percentilesCalculated = true;
	}

	// device one
	public double getMeanXDeviceOneMin() { return meanXDeviceOneMin; }	
	public double getMeanXDeviceOneMax() { return meanXDeviceOneMax; }	
	public double getMeanYDeviceOneMin() { return meanYDeviceOneMin; }	
	public double getMeanYDeviceOneMax() { return meanYDeviceOneMax; }	
	public double getMeanZDeviceOneMin() { return meanZDeviceOneMin; }	
	public double getMeanZDeviceOneMax() { return meanZDeviceOneMax; }

	public double getVarXDeviceOneMin() { return varXDeviceOneMin; }	
	public double getVarXDeviceOneMax() { return varXDeviceOneMax; }	
	public double getVarYDeviceOneMin() { return varYDeviceOneMin; }	
	public double getVarYDeviceOneMax() { return varYDeviceOneMax; }	
	public double getVarZDeviceOneMin() { return varZDeviceOneMin; }	
	public double getVarZDeviceOneMax() { return varZDeviceOneMax; }

	public double getCorXYDeviceOneMin() { return corXYDeviceOneMin; }
	public double getCorXYDeviceOneMax() { return corXYDeviceOneMax; }
	public double getCorYZDeviceOneMin() { return corYZDeviceOneMin; }
	public double getCorYZDeviceOneMax() { return corYZDeviceOneMax; }
	public double getCorZXDeviceOneMin() { return corZXDeviceOneMin; }
	public double getCorZXDeviceOneMax() { return corZXDeviceOneMax; }

	// device two
	public double getMeanXDeviceTwoMin() { return meanXDeviceTwoMin; }	
	public double getMeanXDeviceTwoMax() { return meanXDeviceTwoMax; }	
	public double getMeanYDeviceTwoMin() { return meanYDeviceTwoMin; }	
	public double getMeanYDeviceTwoMax() { return meanYDeviceTwoMax; }	
	public double getMeanZDeviceTwoMin() { return meanZDeviceTwoMin; }	
	public double getMeanZDeviceTwoMax() { return meanZDeviceTwoMax; }

	public double getVarXDeviceTwoMin() { return varXDeviceTwoMin; }	
	public double getVarXDeviceTwoMax() { return varXDeviceTwoMax; }	
	public double getVarYDeviceTwoMin() { return varYDeviceTwoMin; }	
	public double getVarYDeviceTwoMax() { return varYDeviceTwoMax; }	
	public double getVarZDeviceTwoMin() { return varZDeviceTwoMin; }	
	public double getVarZDeviceTwoMax() { return varZDeviceTwoMax; }

	public double getCorXYDeviceTwoMin() { return corXYDeviceTwoMin; }
	public double getCorXYDeviceTwoMax() { return corXYDeviceTwoMax; }
	public double getCorYZDeviceTwoMin() { return corYZDeviceTwoMin; }
	public double getCorYZDeviceTwoMax() { return corYZDeviceTwoMax; }
	public double getCorZXDeviceTwoMin() { return corZXDeviceTwoMin; }
	public double getCorZXDeviceTwoMax() { return corZXDeviceTwoMax; }

	// device three
	public double getMeanXDeviceThreeMin() { return meanXDeviceThreeMin; }	
	public double getMeanXDeviceThreeMax() { return meanXDeviceThreeMax; }	
	public double getMeanYDeviceThreeMin() { return meanYDeviceThreeMin; }	
	public double getMeanYDeviceThreeMax() { return meanYDeviceThreeMax; }	
	public double getMeanZDeviceThreeMin() { return meanZDeviceThreeMin; }	
	public double getMeanZDeviceThreeMax() { return meanZDeviceThreeMax; }

	public double getVarXDeviceThreeMin() { return varXDeviceThreeMin; }	
	public double getVarXDeviceThreeMax() { return varXDeviceThreeMax; }	
	public double getVarYDeviceThreeMin() { return varYDeviceThreeMin; }	
	public double getVarYDeviceThreeMax() { return varYDeviceThreeMax; }	
	public double getVarZDeviceThreeMin() { return varZDeviceThreeMin; }	
	public double getVarZDeviceThreeMax() { return varZDeviceThreeMax; }

	public double getCorXYDeviceThreeMin() { return corXYDeviceThreeMin; }
	public double getCorXYDeviceThreeMax() { return corXYDeviceThreeMax; }
	public double getCorYZDeviceThreeMin() { return corYZDeviceThreeMin; }
	public double getCorYZDeviceThreeMax() { return corYZDeviceThreeMax; }
	public double getCorZXDeviceThreeMin() { return corZXDeviceThreeMin; }
	public double getCorZXDeviceThreeMax() { return corZXDeviceThreeMax; }

	// device four
	public double getMeanXDeviceFourMin() { return meanXDeviceFourMin; }	
	public double getMeanXDeviceFourMax() { return meanXDeviceFourMax; }	
	public double getMeanYDeviceFourMin() { return meanYDeviceFourMin; }	
	public double getMeanYDeviceFourMax() { return meanYDeviceFourMax; }	
	public double getMeanZDeviceFourMin() { return meanZDeviceFourMin; }	
	public double getMeanZDeviceFourMax() { return meanZDeviceFourMax; }

	public double getVarXDeviceFourMin() { return varXDeviceFourMin; }	
	public double getVarXDeviceFourMax() { return varXDeviceFourMax; }	
	public double getVarYDeviceFourMin() { return varYDeviceFourMin; }	
	public double getVarYDeviceFourMax() { return varYDeviceFourMax; }	
	public double getVarZDeviceFourMin() { return varZDeviceFourMin; }	
	public double getVarZDeviceFourMax() { return varZDeviceFourMax; }

	public double getCorXYDeviceFourMin() { return corXYDeviceFourMin; }
	public double getCorXYDeviceFourMax() { return corXYDeviceFourMax; }
	public double getCorYZDeviceFourMin() { return corYZDeviceFourMin; }
	public double getCorYZDeviceFourMax() { return corYZDeviceFourMax; }
	public double getCorZXDeviceFourMin() { return corZXDeviceFourMin; }
	public double getCorZXDeviceFourMax() { return corZXDeviceFourMax; }

	public boolean percentilesComputed() { return percentilesCalculated; }

	public Instances getAllScaledInstances(FastVector schema) {
		//LinkedList<AccelerometerFrameFourDevices> scaledFrames = new LinkedList<AccelerometerFrameFourDevices>();
		Instances scaledInstances = new Instances("scaledUnlabelled", schema, 0);
		for (AccelerometerFrameFourDevices unscaledFrame : unscaledFrames) {
			AccelerometerFrameFourDevices scaledFrame = scaleFrame(unscaledFrame);
			Instance scaledInstance = FrameToInstanceConverter.convert(scaledFrame, scaledInstances);
			scaledInstances.add(scaledInstance);
		}
		return scaledInstances;
	}

	public void dropSignalData() {
		for (AccelerometerFrameFourDevices frame4d : unscaledFrames) {
			frame4d.dropSignalData();
		}
	}

	public void dropStatsIntermediaryData() {		
		// mean
		meanXDeviceOneStats = null;
		meanXDeviceTwoStats = null;
		meanXDeviceThreeStats = null;
		meanXDeviceFourStats = null;

		meanYDeviceOneStats = null;
		meanYDeviceTwoStats = null;
		meanYDeviceThreeStats = null;
		meanYDeviceFourStats = null;

		meanZDeviceOneStats = null;
		meanZDeviceTwoStats = null;
		meanZDeviceThreeStats = null;
		meanZDeviceFourStats = null;

		// var
		varXDeviceOneStats = null;
		varXDeviceTwoStats = null;
		varXDeviceThreeStats = null;
		varXDeviceFourStats = null;

		varYDeviceOneStats = null;
		varYDeviceTwoStats = null;
		varYDeviceThreeStats = null;
		varYDeviceFourStats = null;

		varZDeviceOneStats = null;
		varZDeviceTwoStats = null;
		varZDeviceThreeStats = null;
		varZDeviceFourStats = null;

		// cor		
		corXYDeviceOneStats = null;
		corXYDeviceTwoStats = null;
		corXYDeviceThreeStats = null;
		corXYDeviceFourStats = null;

		corYZDeviceOneStats = null;
		corYZDeviceTwoStats = null;
		corYZDeviceThreeStats = null;
		corYZDeviceFourStats = null;

		corZXDeviceOneStats = null;
		corZXDeviceTwoStats = null;
		corZXDeviceThreeStats = null;
		corZXDeviceFourStats = null;
	}
}

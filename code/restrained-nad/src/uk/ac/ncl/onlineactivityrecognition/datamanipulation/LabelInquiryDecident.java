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

import java.util.Arrays;

import android.util.Log;

import weka.classifiers.Classifier;


public class LabelInquiryDecident {

	protected double[] probDistr;
	protected double[] probDistrGeom;
	
	protected int[] countDistr;
	protected int numFrames;

	protected double sumConfidence;
	protected double sumMargin;
	
	protected double productConfidence = 1;
	protected double productMargin = 1;

	protected int[] classesBin;

	private int indexOfMaxDouble(double[] v) {
		int iMax = 0;
		double max = v[iMax];

		for (int i = 1; i < v.length; i++) {
			if (v[i] > max) {
				iMax = i;
				max = v[i];
			}
		}

		return iMax;
	}

	private int indexOfMaxInt(int[] v) {
		int iMax = 0;
		int max = v[iMax];

		for (int i = 1; i < v.length; i++) {
			if (v[i] > max) {
				iMax = i;
				max = v[i];
			}
		}

		return iMax;
	}

	public void reset() {
		probDistr = null;
		probDistrGeom = null;
		
		countDistr = null;
		
		sumConfidence = 0;
		sumMargin = 0;
		
		productConfidence = 1;
		productMargin = 1;
		
		numFrames = 0;
	}

	public void addProb(double[] probDistr, int n) {
		for (int k = 0; k < probDistr.length; k++) {
			Log.i("LID", "(" + k + ") " + probDistr[k]);
		}
		
		if (numFrames <= 0) {
			this.probDistr = new double[probDistr.length];
			System.arraycopy(probDistr, 0, this.probDistr, 0, probDistr.length);
			
			probDistrGeom = new double[probDistr.length];
			for (int i = 0; i < probDistr.length; i++) {
				probDistrGeom[i] = Math.pow(probDistr[i], 1f / ((double)n) );
			}
			//System.arraycopy(probDistr, 0, probDistrGeom, 0, probDistr.length);
			
			countDistr = new int[probDistr.length];
		} else {
			for (int i = 0; i < probDistr.length; i++) {
				this.probDistr[i] += probDistr[i];
				probDistrGeom[i] *= Math.pow(probDistr[i], 1f / ((double)n) );
				
			}
		}

		int iMax = indexOfMaxDouble(probDistr);
		countDistr[iMax]++;
		++numFrames;

		Arrays.sort(probDistr);
		int firstIndex = probDistr.length - 1;
		int secondIndex = firstIndex - 1;
		sumConfidence += probDistr[firstIndex];
		sumMargin += (probDistr[firstIndex] - probDistr[secondIndex]);
		
		productConfidence *= Math.pow(probDistr[firstIndex], 1f / ((double)n) );
		// NOTE
		// using division instead of subtraction in geometric mean
		productMargin *= Math.pow(probDistr[firstIndex] / probDistr[secondIndex], 1f / ((double)n) );
	}

	public double marginOfMode() {
		double[] distr = new double[probDistr.length];
		System.arraycopy(probDistr, 0, distr, 0, probDistr.length);
		Arrays.sort(distr);
		int firstIndex = distr.length - 1;
		int secondIndex = firstIndex - 1;
		return (distr[firstIndex] - distr[secondIndex]) / numFrames;
	}
	
	public double marginOfModeGeom() {
		double[] distr = new double[probDistr.length];
		System.arraycopy(probDistrGeom, 0, distr, 0, probDistr.length);
		Arrays.sort(distr);
		int firstIndex = distr.length - 1;
		int secondIndex = firstIndex - 1;
		return (distr[firstIndex] / distr[secondIndex]);
	}

	public double averageMargin() {
		return sumMargin / numFrames;
	}
	
	public double averageMarginGeom() {
		return productMargin;
	}

	public double confidenceOfMode() {
		int iMax = indexOfMaxInt(countDistr);
		return probDistr[iMax] / numFrames;
	}
	
	public double confidenceOfModeGeom() {
		int iMax = indexOfMaxInt(countDistr);
		//return Math.pow(probDistrGeom[iMax], 1f / ((double)numFrames) );
		return probDistrGeom[iMax];
	}

	public double averageConfidence() {
		return sumConfidence / numFrames;
	}
	
	public double averageConfidenceGeom() {
		return productConfidence;
	}

	public enum Metric {
		MARGIN_OF_MODE, 
		MARGIN_OF_MODE_GEOM, 
		AVERAGE_MARGIN,
		AVERAGE_MARGIN_GEOM,
		CONFIDENCE_OF_MODE,
		CONFIDENCE_OF_MODE_GEOM,
		AVERAGE_CONFIDENCE,
		AVERAGE_CONFIDENCE_GEOM
	}
	
	public enum BetaFunction {
		BETA_FUNCTION,
		LOG_BETA_FUNCTION,
	}

	public double askProb(double gamma, Metric metric, BetaFunction betaFunction) {
		double p;
		
		switch (metric) {
		case MARGIN_OF_MODE:
			p = marginOfMode();
			break;
		case MARGIN_OF_MODE_GEOM:
			p = marginOfModeGeom();
			break;
			
		case AVERAGE_MARGIN:
			p = averageMargin();
			break;
		case AVERAGE_MARGIN_GEOM:
			p = averageMarginGeom();
			break;
			
		case CONFIDENCE_OF_MODE:
			p = confidenceOfMode();
			break;
		case CONFIDENCE_OF_MODE_GEOM:
			p = confidenceOfModeGeom();
			break;
			
		case AVERAGE_CONFIDENCE:
			p = averageConfidence();
			break;
		case AVERAGE_CONFIDENCE_GEOM:
			p = averageConfidenceGeom();
			break;
			
		default:
			p = 0;
			break;
		}

		Log.i("LID", "P_class = " + p);
		
		if (betaFunction == BetaFunction.BETA_FUNCTION) {
			return betaFunctionScaled(gamma, p);
		} else {
			return betaLogFunctionScaled(gamma, p); 
		}
	}
	
	public int getIndexOfMode() {
		return indexOfMaxInt(countDistr);
	}
	
	private double betaFunction(double gamma, double p) {
		return Math.exp(-gamma * p);
	}	
	private double betaFunctionScaled(double gamma, double p) {
		return (betaFunction(gamma, p) - betaFunction(gamma, 1)) / (betaFunction(gamma, 0) - betaFunction(gamma, 1));
	}
	
	private double betaLogFunction(double gamma, double p) {
		return (1 - Math.exp(gamma * Math.log(p)) );
	}
	private double betaLogFunctionScaled(double gamma, double p) {
		return (betaLogFunction(gamma, p) - betaLogFunction(gamma, 1) / 
				(betaLogFunction(gamma, 1e-75) - betaLogFunction(gamma, 1)) );
	}

}

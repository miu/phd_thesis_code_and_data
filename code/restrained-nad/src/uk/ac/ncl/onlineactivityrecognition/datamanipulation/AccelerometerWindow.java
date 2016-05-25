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

public class AccelerometerWindow implements Serializable {
	
	private static final long serialVersionUID = 1557015537419827643L;
	
	LinkedList<AccelerometerSample> window = new LinkedList<AccelerometerSample>();
	
	public AccelerometerWindow() {
		setInitialValues();
	}
	
	public AccelerometerSample getFirstSample() {
		return window.peekFirst();
	}
	
	public AccelerometerSample getLastSample() {
		return window.peekLast();
	}
	
	// auxiliary measures for computing features incrementally
	// sum of samples
	private double sumX;
	private double sumY;
	private double sumZ;
	public double getSumX() { return sumX; }
	public double getSumY() { return sumY; }
	public double getSumZ() { return sumZ; }
	
	// sum of squares of samples
	private double sumSqX;
	private double sumSqY;
	private double sumSqZ;
	public double getSumSqX() { return sumSqX; }
	public double getSumSqY() { return sumSqY; }
	public double getSumSqZ() { return sumSqZ; }
	
	// sum of cross-axis products
	private double sumXY;
	private double sumYZ;
	private double sumZX;
	public double getSumXY() { return sumXY; }
	public double getSumYZ() { return sumYZ; }
	public double getSumZX() { return sumZX; }
	
	// features
	// mean
	private double meanX;
	private double meanY;
	private double meanZ;
	public double getMeanX() { return meanX; }
	public double getMeanY() { return meanY; }
	public double getMeanZ() { return meanZ; }
	public void setMeanX(double meanX) { this.meanX = meanX; }
	public void setMeanY(double meanY) { this.meanY = meanY; }
	public void setMeanZ(double meanZ) { this.meanZ = meanZ; }
	
	// variances
	private double varX;
	private double varY;
	private double varZ;
	public double getVarX() { return varX; }
	public double getVarY() { return varY; }
	public double getVarZ() { return varZ; }
	public void setVarX(double varX) { this.varX = varX; }
	public void setVarY(double varY) { this.varY = varY; }
	public void setVarZ(double varZ) { this.varZ = varZ; }
	
	// cross-axis correlations
	private double corXY;
	private double corYZ;
	private double corZX;
	public double getCorXY() { return corXY; }
	public double getCorYZ() { return corYZ; }
	public double getCorZX() { return corZX; }
	public void setCorXY(double corXY) { this.corXY = corXY; }
	public void setCorYZ(double corYZ) { this.corYZ = corYZ; }
	public void setCorZX(double corZX) { this.corZX = corZX; }
	
	private boolean finalized;
	
	public void addSample(AccelerometerSample sample) { 
		window.add(sample); 
		
		double x = sample.getX();
		double y = sample.getY();
		double z = sample.getZ();
		
		// update sums
		sumX += x;
		sumY += y;
		sumZ += z;
		
		// update sums of squares
		sumSqX += x*x;
		sumSqY += y*y;
		sumSqZ += z*z;
		
		// update sums of products
		sumXY += x*y;
		sumYZ += y*z;
		sumZX += z*x;		
	}
	
	public int getSize() { return window.size(); }
	
	public void finalizeFeatures() {
		int size = getSize();
		finalized = true;
		
		// means		
		meanX = sumX / size;
		meanY = sumY / size;
		meanZ = sumZ / size;
		
		// variances: E[X^2] - E^2[X]
		varX = (sumSqX / size) - meanX*meanX;
		varY = (sumSqY / size) - meanY*meanY;
		varZ = (sumSqZ / size) - meanZ*meanZ;
				
		// cross-axis covariance: E[XY] - E[X]*E[Y]
		double covXY = (sumXY / size) - meanX*meanY;
		double covYZ = (sumYZ / size) - meanY*meanZ;
		double covZX = (sumZX / size) - meanZ*meanX;
		
		// cross-axis correlations
		// zero if variance is zero
		corXY = Math.abs(varX*varY) > 0 ? covXY / (varX*varY) : 0;
		corYZ = Math.abs(varY*varZ) > 0 ? covYZ / (varY*varZ) : 0;
		corZX = Math.abs(varZ*varX) > 0 ? covZX / (varZ*varX) : 0;
	}

	public void clear() {
		window.clear();
		setInitialValues();
	}

	private void setInitialValues() {
		sumX = 0;
		sumY = 0;
		sumZ = 0;
		
		sumSqX = 0;
		sumSqY = 0;
		sumSqZ = 0;
		
		sumXY = 0;
		sumYZ = 0;
		sumZX = 0;
		
		
		meanX = Double.NaN;
		meanY = Double.NaN;
		meanZ = Double.NaN;
		
		varX = Double.NaN;
		varY = Double.NaN;
		varZ = Double.NaN;
		
		corXY = Double.NaN;
		corYZ = Double.NaN;
		corZX = Double.NaN;
		
		finalized = false;
	}
	
	public boolean isFinalized() { return finalized; }
	
	public int getNumSamples() { return window.size(); }
}

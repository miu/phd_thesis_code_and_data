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

public class FeatureRangeCalculatorLowMemory implements Serializable {

	private static final long serialVersionUID = -4678522939270385131L;

	int tailLength;

	// device one
	private double[] meanXDeviceOneMins;
	private double[] meanXDeviceOneMaxs;
	private double[] meanYDeviceOneMins;
	private double[] meanYDeviceOneMaxs;
	private double[] meanZDeviceOneMins;
	private double[] meanZDeviceOneMaxs;

	private double[] varXDeviceOneMins;
	private double[] varXDeviceOneMaxs;
	private double[] varYDeviceOneMins;
	private double[] varYDeviceOneMaxs;
	private double[] varZDeviceOneMins;
	private double[] varZDeviceOneMaxs;

	private double[] corXYDeviceOneMins;
	private double[] corXYDeviceOneMaxs;
	private double[] corYZDeviceOneMins;
	private double[] corYZDeviceOneMaxs;
	private double[] corZXDeviceOneMins;
	private double[] corZXDeviceOneMaxs;

	// device two
	private double[] meanXDeviceTwoMins;
	private double[] meanXDeviceTwoMaxs;
	private double[] meanYDeviceTwoMins;
	private double[] meanYDeviceTwoMaxs;
	private double[] meanZDeviceTwoMins;
	private double[] meanZDeviceTwoMaxs;

	private double[] varXDeviceTwoMins;
	private double[] varXDeviceTwoMaxs;
	private double[] varYDeviceTwoMins;
	private double[] varYDeviceTwoMaxs;
	private double[] varZDeviceTwoMins;
	private double[] varZDeviceTwoMaxs;

	private double[] corXYDeviceTwoMins;
	private double[] corXYDeviceTwoMaxs;
	private double[] corYZDeviceTwoMins;
	private double[] corYZDeviceTwoMaxs;
	private double[] corZXDeviceTwoMins;
	private double[] corZXDeviceTwoMaxs;

	// device three
	private double[] meanXDeviceThreeMins;
	private double[] meanXDeviceThreeMaxs;
	private double[] meanYDeviceThreeMins;
	private double[] meanYDeviceThreeMaxs;
	private double[] meanZDeviceThreeMins;
	private double[] meanZDeviceThreeMaxs;

	private double[] varXDeviceThreeMins;
	private double[] varXDeviceThreeMaxs;
	private double[] varYDeviceThreeMins;
	private double[] varYDeviceThreeMaxs;
	private double[] varZDeviceThreeMins;
	private double[] varZDeviceThreeMaxs;

	private double[] corXYDeviceThreeMins;
	private double[] corXYDeviceThreeMaxs;
	private double[] corYZDeviceThreeMins;
	private double[] corYZDeviceThreeMaxs;
	private double[] corZXDeviceThreeMins;
	private double[] corZXDeviceThreeMaxs;

	// device four
	private double[] meanXDeviceFourMins;
	private double[] meanXDeviceFourMaxs;
	private double[] meanYDeviceFourMins;
	private double[] meanYDeviceFourMaxs;
	private double[] meanZDeviceFourMins;
	private double[] meanZDeviceFourMaxs;

	private double[] varXDeviceFourMins;
	private double[] varXDeviceFourMaxs;
	private double[] varYDeviceFourMins;
	private double[] varYDeviceFourMaxs;
	private double[] varZDeviceFourMins;
	private double[] varZDeviceFourMaxs;

	private double[] corXYDeviceFourMins;
	private double[] corXYDeviceFourMaxs;
	private double[] corYZDeviceFourMins;
	private double[] corYZDeviceFourMaxs;
	private double[] corZXDeviceFourMins;
	private double[] corZXDeviceFourMaxs;

	public FeatureRangeCalculatorLowMemory(int tailLength) {
		this.tailLength = tailLength; 

		// device one
		meanXDeviceOneMins = allocateMins();
		meanXDeviceOneMaxs = allocateMaxs();
		meanYDeviceOneMins = allocateMins();
		meanYDeviceOneMaxs = allocateMaxs();
		meanZDeviceOneMins = allocateMins();
		meanZDeviceOneMaxs = allocateMaxs();

		varXDeviceOneMins = allocateMins();
		varXDeviceOneMaxs = allocateMaxs();
		varYDeviceOneMins = allocateMins();
		varYDeviceOneMaxs = allocateMaxs();
		varZDeviceOneMins = allocateMins();
		varZDeviceOneMaxs = allocateMaxs();

		corXYDeviceOneMins = allocateMins();
		corXYDeviceOneMaxs = allocateMaxs();
		corYZDeviceOneMins = allocateMins();
		corYZDeviceOneMaxs = allocateMaxs();
		corZXDeviceOneMins = allocateMins();
		corZXDeviceOneMaxs = allocateMaxs();

		// device two
		meanXDeviceTwoMins = allocateMins();
		meanXDeviceTwoMaxs = allocateMaxs();
		meanYDeviceTwoMins = allocateMins();
		meanYDeviceTwoMaxs = allocateMaxs();
		meanZDeviceTwoMins = allocateMins();
		meanZDeviceTwoMaxs = allocateMaxs();

		varXDeviceTwoMins = allocateMins();
		varXDeviceTwoMaxs = allocateMaxs();
		varYDeviceTwoMins = allocateMins();
		varYDeviceTwoMaxs = allocateMaxs();
		varZDeviceTwoMins = allocateMins();
		varZDeviceTwoMaxs = allocateMaxs();

		corXYDeviceTwoMins = allocateMins();
		corXYDeviceTwoMaxs = allocateMaxs();
		corYZDeviceTwoMins = allocateMins();
		corYZDeviceTwoMaxs = allocateMaxs();
		corZXDeviceTwoMins = allocateMins();
		corZXDeviceTwoMaxs = allocateMaxs();

		// device three
		meanXDeviceThreeMins = allocateMins();
		meanXDeviceThreeMaxs = allocateMaxs();
		meanYDeviceThreeMins = allocateMins();
		meanYDeviceThreeMaxs = allocateMaxs();
		meanZDeviceThreeMins = allocateMins();
		meanZDeviceThreeMaxs = allocateMaxs();

		varXDeviceThreeMins = allocateMins();
		varXDeviceThreeMaxs = allocateMaxs();
		varYDeviceThreeMins = allocateMins();
		varYDeviceThreeMaxs = allocateMaxs();
		varZDeviceThreeMins = allocateMins();
		varZDeviceThreeMaxs = allocateMaxs();

		corXYDeviceThreeMins = allocateMins();
		corXYDeviceThreeMaxs = allocateMaxs();
		corYZDeviceThreeMins = allocateMins();
		corYZDeviceThreeMaxs = allocateMaxs();
		corZXDeviceThreeMins = allocateMins();
		corZXDeviceThreeMaxs = allocateMaxs();

		// device four
		meanXDeviceFourMins = allocateMins();
		meanXDeviceFourMaxs = allocateMaxs();
		meanYDeviceFourMins = allocateMins();
		meanYDeviceFourMaxs = allocateMaxs();
		meanZDeviceFourMins = allocateMins();
		meanZDeviceFourMaxs = allocateMaxs();

		varXDeviceFourMins = allocateMins();
		varXDeviceFourMaxs = allocateMaxs();
		varYDeviceFourMins = allocateMins();
		varYDeviceFourMaxs = allocateMaxs();
		varZDeviceFourMins = allocateMins();
		varZDeviceFourMaxs = allocateMaxs();

		corXYDeviceFourMins = allocateMins();
		corXYDeviceFourMaxs = allocateMaxs();
		corYZDeviceFourMins = allocateMins();
		corYZDeviceFourMaxs = allocateMaxs();
		corZXDeviceFourMins = allocateMins();
		corZXDeviceFourMaxs = allocateMaxs();
	}

	public void add(AccelerometerFrameFourDevices frame4d) {

		// device one
		double meanXDeviceOne = frame4d.getMeanXDeviceOne();
		insertMin(meanXDeviceOne, meanXDeviceOneMins);
		insertMax(meanXDeviceOne, meanXDeviceOneMaxs);
		double meanYDeviceOne = frame4d.getMeanYDeviceOne();
		insertMin(meanYDeviceOne, meanYDeviceOneMins);
		insertMax(meanYDeviceOne, meanYDeviceOneMaxs);
		double meanZDeviceOne = frame4d.getMeanZDeviceOne();
		insertMin(meanZDeviceOne, meanZDeviceOneMins);
		insertMax(meanZDeviceOne, meanZDeviceOneMaxs);

		double varXDeviceOne = frame4d.getVarXDeviceOne();
		insertMin(varXDeviceOne, varXDeviceOneMins);
		insertMax(varXDeviceOne, varXDeviceOneMaxs);
		double varYDeviceOne = frame4d.getVarYDeviceOne();
		insertMin(varYDeviceOne, varYDeviceOneMins);
		insertMax(varYDeviceOne, varYDeviceOneMaxs);
		double varZDeviceOne = frame4d.getVarZDeviceOne();
		insertMin(varZDeviceOne, varZDeviceOneMins);
		insertMax(varZDeviceOne, varZDeviceOneMaxs);

		double corXYDeviceOne = frame4d.getCorXYDeviceOne();
		insertMin(corXYDeviceOne, corXYDeviceOneMins);
		insertMax(corXYDeviceOne, corXYDeviceOneMaxs);
		double corYZDeviceOne = frame4d.getCorYZDeviceOne();
		insertMin(corYZDeviceOne, corYZDeviceOneMins);
		insertMax(corYZDeviceOne, corYZDeviceOneMaxs);
		double corZXDeviceOne = frame4d.getCorZXDeviceOne();
		insertMin(corZXDeviceOne, corZXDeviceOneMins);
		insertMax(corZXDeviceOne, corZXDeviceOneMaxs);

		// device two
		double meanXDeviceTwo = frame4d.getMeanXDeviceTwo();
		insertMin(meanXDeviceTwo, meanXDeviceTwoMins);
		insertMax(meanXDeviceTwo, meanXDeviceTwoMaxs);
		double meanYDeviceTwo = frame4d.getMeanYDeviceTwo();
		insertMin(meanYDeviceTwo, meanYDeviceTwoMins);
		insertMax(meanYDeviceTwo, meanYDeviceTwoMaxs);
		double meanZDeviceTwo = frame4d.getMeanZDeviceTwo();
		insertMin(meanZDeviceTwo, meanZDeviceTwoMins);
		insertMax(meanZDeviceTwo, meanZDeviceTwoMaxs);

		double varXDeviceTwo = frame4d.getVarXDeviceTwo();
		insertMin(varXDeviceTwo, varXDeviceTwoMins);
		insertMax(varXDeviceTwo, varXDeviceTwoMaxs);
		double varYDeviceTwo = frame4d.getVarYDeviceTwo();
		insertMin(varYDeviceTwo, varYDeviceTwoMins);
		insertMax(varYDeviceTwo, varYDeviceTwoMaxs);
		double varZDeviceTwo = frame4d.getVarZDeviceTwo();
		insertMin(varZDeviceTwo, varZDeviceTwoMins);
		insertMax(varZDeviceTwo, varZDeviceTwoMaxs);

		double corXYDeviceTwo = frame4d.getCorXYDeviceTwo();
		insertMin(corXYDeviceTwo, corXYDeviceTwoMins);
		insertMax(corXYDeviceTwo, corXYDeviceTwoMaxs);
		double corYZDeviceTwo = frame4d.getCorYZDeviceTwo();
		insertMin(corYZDeviceTwo, corYZDeviceTwoMins);
		insertMax(corYZDeviceTwo, corYZDeviceTwoMaxs);
		double corZXDeviceTwo = frame4d.getCorZXDeviceTwo();
		insertMin(corZXDeviceTwo, corZXDeviceTwoMins);
		insertMax(corZXDeviceTwo, corZXDeviceTwoMaxs);

		// device three
		double meanXDeviceThree = frame4d.getMeanXDeviceThree();
		insertMin(meanXDeviceThree, meanXDeviceThreeMins);
		insertMax(meanXDeviceThree, meanXDeviceThreeMaxs);
		double meanYDeviceThree = frame4d.getMeanYDeviceThree();
		insertMin(meanYDeviceThree, meanYDeviceThreeMins);
		insertMax(meanYDeviceThree, meanYDeviceThreeMaxs);
		double meanZDeviceThree = frame4d.getMeanZDeviceThree();
		insertMin(meanZDeviceThree, meanZDeviceThreeMins);
		insertMax(meanZDeviceThree, meanZDeviceThreeMaxs);

		double varXDeviceThree = frame4d.getVarXDeviceThree();
		insertMin(varXDeviceThree, varXDeviceThreeMins);
		insertMax(varXDeviceThree, varXDeviceThreeMaxs);
		double varYDeviceThree = frame4d.getVarYDeviceThree();
		insertMin(varYDeviceThree, varYDeviceThreeMins);
		insertMax(varYDeviceThree, varYDeviceThreeMaxs);
		double varZDeviceThree = frame4d.getVarZDeviceThree();
		insertMin(varZDeviceThree, varZDeviceThreeMins);
		insertMax(varZDeviceThree, varZDeviceThreeMaxs);

		double corXYDeviceThree = frame4d.getCorXYDeviceThree();
		insertMin(corXYDeviceThree, corXYDeviceThreeMins);
		insertMax(corXYDeviceThree, corXYDeviceThreeMaxs);
		double corYZDeviceThree = frame4d.getCorYZDeviceThree();
		insertMin(corYZDeviceThree, corYZDeviceThreeMins);
		insertMax(corYZDeviceThree, corYZDeviceThreeMaxs);
		double corZXDeviceThree = frame4d.getCorZXDeviceThree();
		insertMin(corZXDeviceThree, corZXDeviceThreeMins);
		insertMax(corZXDeviceThree, corZXDeviceThreeMaxs);

		// device four
		double meanXDeviceFour = frame4d.getMeanXDeviceFour();
		insertMin(meanXDeviceFour, meanXDeviceFourMins);
		insertMax(meanXDeviceFour, meanXDeviceFourMaxs);
		double meanYDeviceFour = frame4d.getMeanYDeviceFour();
		insertMin(meanYDeviceFour, meanYDeviceFourMins);
		insertMax(meanYDeviceFour, meanYDeviceFourMaxs);
		double meanZDeviceFour = frame4d.getMeanZDeviceFour();
		insertMin(meanZDeviceFour, meanZDeviceFourMins);
		insertMax(meanZDeviceFour, meanZDeviceFourMaxs);

		double varXDeviceFour = frame4d.getVarXDeviceFour();
		insertMin(varXDeviceFour, varXDeviceFourMins);
		insertMax(varXDeviceFour, varXDeviceFourMaxs);
		double varYDeviceFour = frame4d.getVarYDeviceFour();
		insertMin(varYDeviceFour, varYDeviceFourMins);
		insertMax(varYDeviceFour, varYDeviceFourMaxs);
		double varZDeviceFour = frame4d.getVarZDeviceFour();
		insertMin(varZDeviceFour, varZDeviceFourMins);
		insertMax(varZDeviceFour, varZDeviceFourMaxs);

		double corXYDeviceFour = frame4d.getCorXYDeviceFour();
		insertMin(corXYDeviceFour, corXYDeviceFourMins);
		insertMax(corXYDeviceFour, corXYDeviceFourMaxs);
		double corYZDeviceFour = frame4d.getCorYZDeviceFour();
		insertMin(corYZDeviceFour, corYZDeviceFourMins);
		insertMax(corYZDeviceFour, corYZDeviceFourMaxs);
		double corZXDeviceFour = frame4d.getCorZXDeviceFour();
		insertMin(corZXDeviceFour, corZXDeviceFourMins);
		insertMax(corZXDeviceFour, corZXDeviceFourMaxs);
	}

	public AccelerometerFrameFourDevices scaleFrame(AccelerometerFrameFourDevices frame4d) {
		long tStart = frame4d.getStartTimestamp();
		long tEnd = frame4d.getEndTimestamp();

		//device one
		AccelerometerFrame scaledFrameDeviceOne = new AccelerometerFrame(tStart, tEnd);

		double meanXDeviceOne = frame4d.getMeanXDeviceOne();
		scaledFrameDeviceOne.setMeanX(scale(meanXDeviceOneMins[0], meanXDeviceOne, meanXDeviceOneMaxs[0]));		
		double meanYDeviceOne = frame4d.getMeanYDeviceOne();
		scaledFrameDeviceOne.setMeanY(scale(meanYDeviceOneMins[0], meanYDeviceOne, meanYDeviceOneMaxs[0]));		
		double meanZDeviceOne = frame4d.getMeanZDeviceOne();
		scaledFrameDeviceOne.setMeanZ(scale(meanZDeviceOneMins[0], meanZDeviceOne, meanZDeviceOneMaxs[0]));

		double varXDeviceOne = frame4d.getVarXDeviceOne();
		scaledFrameDeviceOne.setVarX(scale(varXDeviceOneMins[0], varXDeviceOne, varXDeviceOneMaxs[0]));
		double varYDeviceOne = frame4d.getVarYDeviceOne();
		scaledFrameDeviceOne.setVarY(scale(varYDeviceOneMins[0], varYDeviceOne, varYDeviceOneMaxs[0]));
		double varZDeviceOne = frame4d.getVarZDeviceOne();
		scaledFrameDeviceOne.setVarZ(scale(varZDeviceOneMins[0], varZDeviceOne, varZDeviceOneMaxs[0]));

		double corXYDeviceOne = frame4d.getCorXYDeviceOne();
		scaledFrameDeviceOne.setCorXY(scale(corXYDeviceOneMins[0], corXYDeviceOne, corXYDeviceOneMaxs[0]));
		double corYZDeviceOne = frame4d.getCorYZDeviceOne();
		scaledFrameDeviceOne.setCorYZ(scale(corYZDeviceOneMins[0], corYZDeviceOne, corYZDeviceOneMaxs[0]));
		double corZXDeviceOne = frame4d.getCorZXDeviceOne();
		scaledFrameDeviceOne.setCorZX(scale(corZXDeviceOneMins[0], corZXDeviceOne, corZXDeviceOneMaxs[0]));

		scaledFrameDeviceOne.setT(frame4d.getTDeviceOne());
		scaledFrameDeviceOne.setFilled(false);

		//device two
		AccelerometerFrame scaledFrameDeviceTwo = new AccelerometerFrame(tStart, tEnd);

		double meanXDeviceTwo = frame4d.getMeanXDeviceTwo();
		scaledFrameDeviceTwo.setMeanX(scale(meanXDeviceTwoMins[0], meanXDeviceTwo, meanXDeviceTwoMaxs[0]));		
		double meanYDeviceTwo = frame4d.getMeanYDeviceTwo();
		scaledFrameDeviceTwo.setMeanY(scale(meanYDeviceTwoMins[0], meanYDeviceTwo, meanYDeviceTwoMaxs[0]));		
		double meanZDeviceTwo = frame4d.getMeanZDeviceTwo();
		scaledFrameDeviceTwo.setMeanZ(scale(meanZDeviceTwoMins[0], meanZDeviceTwo, meanZDeviceTwoMaxs[0]));

		double varXDeviceTwo = frame4d.getVarXDeviceTwo();
		scaledFrameDeviceTwo.setVarX(scale(varXDeviceTwoMins[0], varXDeviceTwo, varXDeviceTwoMaxs[0]));
		double varYDeviceTwo = frame4d.getVarYDeviceTwo();
		scaledFrameDeviceTwo.setVarY(scale(varYDeviceTwoMins[0], varYDeviceTwo, varYDeviceTwoMaxs[0]));
		double varZDeviceTwo = frame4d.getVarZDeviceTwo();
		scaledFrameDeviceTwo.setVarZ(scale(varZDeviceTwoMins[0], varZDeviceTwo, varZDeviceTwoMaxs[0]));

		double corXYDeviceTwo = frame4d.getCorXYDeviceTwo();
		scaledFrameDeviceTwo.setCorXY(scale(corXYDeviceTwoMins[0], corXYDeviceTwo, corXYDeviceTwoMaxs[0]));
		double corYZDeviceTwo = frame4d.getCorYZDeviceTwo();
		scaledFrameDeviceTwo.setCorYZ(scale(corYZDeviceTwoMins[0], corYZDeviceTwo, corYZDeviceTwoMaxs[0]));
		double corZXDeviceTwo = frame4d.getCorZXDeviceTwo();
		scaledFrameDeviceTwo.setCorZX(scale(corZXDeviceTwoMins[0], corZXDeviceTwo, corZXDeviceTwoMaxs[0]));

		scaledFrameDeviceTwo.setT(frame4d.getTDeviceTwo());
		scaledFrameDeviceTwo.setFilled(false);

		//device three
		AccelerometerFrame scaledFrameDeviceThree = new AccelerometerFrame(tStart, tEnd);

		double meanXDeviceThree = frame4d.getMeanXDeviceThree();
		scaledFrameDeviceThree.setMeanX(scale(meanXDeviceThreeMins[0], meanXDeviceThree, meanXDeviceThreeMaxs[0]));		
		double meanYDeviceThree = frame4d.getMeanYDeviceThree();
		scaledFrameDeviceThree.setMeanY(scale(meanYDeviceThreeMins[0], meanYDeviceThree, meanYDeviceThreeMaxs[0]));		
		double meanZDeviceThree = frame4d.getMeanZDeviceThree();
		scaledFrameDeviceThree.setMeanZ(scale(meanZDeviceThreeMins[0], meanZDeviceThree, meanZDeviceThreeMaxs[0]));

		double varXDeviceThree = frame4d.getVarXDeviceThree();
		scaledFrameDeviceThree.setVarX(scale(varXDeviceThreeMins[0], varXDeviceThree, varXDeviceThreeMaxs[0]));
		double varYDeviceThree = frame4d.getVarYDeviceThree();
		scaledFrameDeviceThree.setVarY(scale(varYDeviceThreeMins[0], varYDeviceThree, varYDeviceThreeMaxs[0]));
		double varZDeviceThree = frame4d.getVarZDeviceThree();
		scaledFrameDeviceThree.setVarZ(scale(varZDeviceThreeMins[0], varZDeviceThree, varZDeviceThreeMaxs[0]));

		double corXYDeviceThree = frame4d.getCorXYDeviceThree();
		scaledFrameDeviceThree.setCorXY(scale(corXYDeviceThreeMins[0], corXYDeviceThree, corXYDeviceThreeMaxs[0]));
		double corYZDeviceThree = frame4d.getCorYZDeviceThree();
		scaledFrameDeviceThree.setCorYZ(scale(corYZDeviceThreeMins[0], corYZDeviceThree, corYZDeviceThreeMaxs[0]));
		double corZXDeviceThree = frame4d.getCorZXDeviceThree();
		scaledFrameDeviceThree.setCorZX(scale(corZXDeviceThreeMins[0], corZXDeviceThree, corZXDeviceThreeMaxs[0]));

		scaledFrameDeviceThree.setT(frame4d.getTDeviceThree());
		scaledFrameDeviceThree.setFilled(false);

		//device four
		AccelerometerFrame scaledFrameDeviceFour = new AccelerometerFrame(tStart, tEnd);

		double meanXDeviceFour = frame4d.getMeanXDeviceFour();
		scaledFrameDeviceFour.setMeanX(scale(meanXDeviceFourMins[0], meanXDeviceFour, meanXDeviceFourMaxs[0]));		
		double meanYDeviceFour = frame4d.getMeanYDeviceFour();
		scaledFrameDeviceFour.setMeanY(scale(meanYDeviceFourMins[0], meanYDeviceFour, meanYDeviceFourMaxs[0]));		
		double meanZDeviceFour = frame4d.getMeanZDeviceFour();
		scaledFrameDeviceFour.setMeanZ(scale(meanZDeviceFourMins[0], meanZDeviceFour, meanZDeviceFourMaxs[0]));

		double varXDeviceFour = frame4d.getVarXDeviceFour();
		scaledFrameDeviceFour.setVarX(scale(varXDeviceFourMins[0], varXDeviceFour, varXDeviceFourMaxs[0]));
		double varYDeviceFour = frame4d.getVarYDeviceFour();
		scaledFrameDeviceFour.setVarY(scale(varYDeviceFourMins[0], varYDeviceFour, varYDeviceFourMaxs[0]));
		double varZDeviceFour = frame4d.getVarZDeviceFour();
		scaledFrameDeviceFour.setVarZ(scale(varZDeviceFourMins[0], varZDeviceFour, varZDeviceFourMaxs[0]));

		double corXYDeviceFour = frame4d.getCorXYDeviceFour();
		scaledFrameDeviceFour.setCorXY(scale(corXYDeviceFourMins[0], corXYDeviceFour, corXYDeviceFourMaxs[0]));
		double corYZDeviceFour = frame4d.getCorYZDeviceFour();
		scaledFrameDeviceFour.setCorYZ(scale(corYZDeviceFourMins[0], corYZDeviceFour, corYZDeviceFourMaxs[0]));
		double corZXDeviceFour = frame4d.getCorZXDeviceFour();
		scaledFrameDeviceFour.setCorZX(scale(corZXDeviceFourMins[0], corZXDeviceFour, corZXDeviceFourMaxs[0]));

		scaledFrameDeviceFour.setT(frame4d.getTDeviceFour());
		scaledFrameDeviceFour.setFilled(false);
		
		return new AccelerometerFrameFourDevices(scaledFrameDeviceOne, scaledFrameDeviceTwo, 
				scaledFrameDeviceThree, scaledFrameDeviceFour);
	}

	private double scale(double min, double actual, double max) {
		double v = (actual - min) / (max - min);
		if (Double.isNaN(v)) {
			return 0.5;
		} else {
			return v < 1 ? v : 1;
		} 
	}

	private double[] allocateMins() {
		double[] v = new double[tailLength];
		for (int i = 0; i < tailLength; i++) {
			v[i] = Double.POSITIVE_INFINITY;
		}
		return v;
	}
	private double[] allocateMaxs() {
		double[] v = new double[tailLength];
		for (int i = 0; i < tailLength; i++) {
			v[i] = Double.NEGATIVE_INFINITY;
		}
		return v;
	}

	private void insertMin(double min, double[] v) {
		int i;
		int n = v.length; 

		for (i = 0; i < n; i++) {
			if (min < v[i]) {
				break;
			}
		}

		if (i >= (n - 1)) {
			return;
		}

		for (int k = n-2; k >= i; k--) {
			v[k+1] = v[k];
		}
		v[i] = min;
	}

	private void insertMax(double max, double[] v) {
		int i;
		int n = v.length;

		for (i = 0; i < n; i++) {
			if (max > v[i]) {
				break;
			}
		}

		if (i >= (n - 1)) {
			return;
		}

		for (int k = n-2; k >= i; k--) {
			v[k+1] = v[k];
		}
		v[i] = max;
	}

}

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

import weka.core.Instance;
import weka.core.Instances;

public class AccelerometerFrameFourDevices implements Serializable {
	
	private static final long serialVersionUID = -8219074791695863038L;
	
	AccelerometerFrame frameDeviceOne;
	AccelerometerFrame frameDeviceTwo;
	AccelerometerFrame frameDeviceThree;
	AccelerometerFrame frameDeviceFour;
	
	public AccelerometerFrameFourDevices(AccelerometerFrame frameDeviceOne, AccelerometerFrame frameDeviceTwo, 
			AccelerometerFrame frameDeviceThree, AccelerometerFrame frameDeviceFour) {
		this.frameDeviceOne = frameDeviceOne;
		this.frameDeviceTwo = frameDeviceTwo;
		this.frameDeviceThree = frameDeviceThree;
		this.frameDeviceFour = frameDeviceFour;
	}
	
	public long getStartTimestamp() { 
		long t1 = frameDeviceOne.getStartTimestamp(); 
		long t2 = frameDeviceTwo.getStartTimestamp();
		long t3 = frameDeviceThree.getStartTimestamp();
		long t4 = frameDeviceFour.getStartTimestamp();
		
		if ((t1 == t2) && (t2 == t3) && (t3 == t4) && (t4 == t1)) {
			return t1;
		} else {
			throw new RuntimeException("Unsynchronized start timestamps: " + 
					t1 + " " + t2 + " " + t3 + " " + t4);
		}		
	}
	public long getEndTimestamp() {
		long t1 = frameDeviceOne.getEndTimestamp(); 
		long t2 = frameDeviceTwo.getEndTimestamp();
		long t3 = frameDeviceThree.getEndTimestamp();
		long t4 = frameDeviceFour.getEndTimestamp();
		
		if ((t1 == t2) && (t2 == t3) && (t3 == t4) && (t4 == t1)) {
			return t1;
		} else {
			throw new RuntimeException("Unsynchronized start timestamps: " + 
					t1 + " " + t2 + " " + t3 + " " + t4);
		}
	}
	
	public long getDeviceOneFirstTimestamp() { return frameDeviceOne.getFirstTimestamp(); }
	public long getDeviceOneLastTimestamp() { return frameDeviceOne.getLastTimestamp(); }
	
	public long getDeviceTwoFirstTimestamp() { return frameDeviceTwo.getFirstTimestamp(); }
	public long getDeviceTwoLastTimestamp() { return frameDeviceTwo.getLastTimestamp(); }
	
	public long getDeviceThreeFirstTimestamp() { return frameDeviceThree.getFirstTimestamp(); }
	public long getDeviceThreeLastTimestamp() { return frameDeviceThree.getLastTimestamp(); }
	
	public long getDeviceFourFirstTimestamp() { return frameDeviceFour.getFirstTimestamp(); }
	public long getDeviceFourLastTimestamp() { return frameDeviceFour.getLastTimestamp(); }
	
	public void computeFeatures() {
		frameDeviceOne.computeFeatures();
		frameDeviceTwo.computeFeatures();
		frameDeviceThree.computeFeatures();
		frameDeviceFour.computeFeatures();
	}
	
	public double getMeanXDeviceOne() { return frameDeviceOne.getMeanX(); }
	public double getMeanYDeviceOne() { return frameDeviceOne.getMeanY(); }
	public double getMeanZDeviceOne() { return frameDeviceOne.getMeanZ(); }	
	public double getVarXDeviceOne() { return frameDeviceOne.getVarX(); }
	public double getVarYDeviceOne() { return frameDeviceOne.getVarY(); }
	public double getVarZDeviceOne() { return frameDeviceOne.getVarZ(); }	
	public double getCorXYDeviceOne() { return frameDeviceOne.getCorXY(); }
	public double getCorYZDeviceOne() { return frameDeviceOne.getCorYZ(); }
	public double getCorZXDeviceOne() { return frameDeviceOne.getCorZX(); }
	
	public double getMeanXDeviceTwo() { return frameDeviceTwo.getMeanX(); }
	public double getMeanYDeviceTwo() { return frameDeviceTwo.getMeanY(); }
	public double getMeanZDeviceTwo() { return frameDeviceTwo.getMeanZ(); }	
	public double getVarXDeviceTwo() { return frameDeviceTwo.getVarX(); }
	public double getVarYDeviceTwo() { return frameDeviceTwo.getVarY(); }
	public double getVarZDeviceTwo() { return frameDeviceTwo.getVarZ(); }	
	public double getCorXYDeviceTwo() { return frameDeviceTwo.getCorXY(); }
	public double getCorYZDeviceTwo() { return frameDeviceTwo.getCorYZ(); }
	public double getCorZXDeviceTwo() { return frameDeviceTwo.getCorZX(); }
	
	public double getMeanXDeviceThree() { return frameDeviceThree.getMeanX(); }
	public double getMeanYDeviceThree() { return frameDeviceThree.getMeanY(); }
	public double getMeanZDeviceThree() { return frameDeviceThree.getMeanZ(); }	
	public double getVarXDeviceThree() { return frameDeviceThree.getVarX(); }
	public double getVarYDeviceThree() { return frameDeviceThree.getVarY(); }
	public double getVarZDeviceThree() { return frameDeviceThree.getVarZ(); }	
	public double getCorXYDeviceThree() { return frameDeviceThree.getCorXY(); }
	public double getCorYZDeviceThree() { return frameDeviceThree.getCorYZ(); }
	public double getCorZXDeviceThree() { return frameDeviceThree.getCorZX(); }
	
	public double getMeanXDeviceFour() { return frameDeviceFour.getMeanX(); }
	public double getMeanYDeviceFour() { return frameDeviceFour.getMeanY(); }
	public double getMeanZDeviceFour() { return frameDeviceFour.getMeanZ(); }	
	public double getVarXDeviceFour() { return frameDeviceFour.getVarX(); }
	public double getVarYDeviceFour() { return frameDeviceFour.getVarY(); }
	public double getVarZDeviceFour() { return frameDeviceFour.getVarZ(); }	
	public double getCorXYDeviceFour() { return frameDeviceFour.getCorXY(); }
	public double getCorYZDeviceFour() { return frameDeviceFour.getCorYZ(); }
	public double getCorZXDeviceFour() { return frameDeviceFour.getCorZX(); }
	
	public double[] getXDeviceOne() { return frameDeviceOne.getX(); }
	public double[] getYDeviceOne() { return frameDeviceOne.getY(); }
	public double[] getZDeviceOne() { return frameDeviceOne.getZ(); }
	
	public double[] getXDeviceTwo() { return frameDeviceTwo.getX(); }
	public double[] getYDeviceTwo() { return frameDeviceTwo.getY(); }
	public double[] getZDeviceTwo() { return frameDeviceTwo.getZ(); }
	
	public double[] getXDeviceThree() { return frameDeviceThree.getX(); }
	public double[] getYDeviceThree() { return frameDeviceThree.getY(); }
	public double[] getZDeviceThree() { return frameDeviceThree.getZ(); }
	
	public double[] getXDeviceFour() { return frameDeviceFour.getX(); }
	public double[] getYDeviceFour() { return frameDeviceFour.getY(); }
	public double[] getZDeviceFour() { return frameDeviceFour.getZ(); }
	
	public long[] getTDeviceOne() { return frameDeviceOne.getT(); }
	
	public long[] getTDeviceTwo() { return frameDeviceTwo.getT(); }
	
	public long[] getTDeviceThree() { return frameDeviceThree.getT(); }
	
	public long[] getTDeviceFour() { return frameDeviceFour.getT(); }
	
	public Instance toWekaInstance(Instances dataset) {
		return FrameToInstanceConverter.convert(this, dataset);
	}
	
	public void dropSignalData() {
		frameDeviceOne.dropSignalData();
		frameDeviceTwo.dropSignalData();
		frameDeviceThree.dropSignalData();
		frameDeviceFour.dropSignalData();
	}
	
	public AccelerometerFrame getFrameDeviceOne() { return frameDeviceOne; }
	public AccelerometerFrame getFrameDeviceTwo() { return frameDeviceTwo; }
	public AccelerometerFrame getFrameDeviceThree() { return frameDeviceThree; }
	public AccelerometerFrame getFrameDeviceFour() { return frameDeviceFour; }
	
	public AccelerometerFrameFourDevices getCopyWithoutSignalData() {
		return new AccelerometerFrameFourDevices(frameDeviceOne.getCopyWithoutSignalData(), 
				frameDeviceTwo.getCopyWithoutSignalData(), 
				frameDeviceThree.getCopyWithoutSignalData(), 
				frameDeviceFour.getCopyWithoutSignalData());
	}
}

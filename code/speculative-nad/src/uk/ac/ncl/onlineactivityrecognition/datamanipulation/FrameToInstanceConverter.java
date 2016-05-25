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

import weka.core.Instance;
import weka.core.Instances;

public class FrameToInstanceConverter {
	public static Instance convert(AccelerometerFrameFourDevices frame4d, Instances dataset) {
		Instance instance = new Instance(37);
		instance.setDataset(dataset);

		int i = 0;

		// device one
		instance.setValue(i++, frame4d.getMeanXDeviceOne());
		instance.setValue(i++, frame4d.getMeanYDeviceOne());
		instance.setValue(i++, frame4d.getMeanZDeviceOne());
		instance.setValue(i++, frame4d.getVarXDeviceOne());
		instance.setValue(i++, frame4d.getVarYDeviceOne());
		instance.setValue(i++, frame4d.getVarZDeviceOne());
		instance.setValue(i++, frame4d.getCorXYDeviceOne());
		instance.setValue(i++, frame4d.getCorYZDeviceOne());
		instance.setValue(i++, frame4d.getCorZXDeviceOne());

		// device two
		instance.setValue(i++, frame4d.getMeanXDeviceTwo());
		instance.setValue(i++, frame4d.getMeanYDeviceTwo());
		instance.setValue(i++, frame4d.getMeanZDeviceTwo());
		instance.setValue(i++, frame4d.getVarXDeviceTwo());
		instance.setValue(i++, frame4d.getVarYDeviceTwo());
		instance.setValue(i++, frame4d.getVarZDeviceTwo());
		instance.setValue(i++, frame4d.getCorXYDeviceTwo());
		instance.setValue(i++, frame4d.getCorYZDeviceTwo());
		instance.setValue(i++, frame4d.getCorZXDeviceTwo());

		// device three
		instance.setValue(i++, frame4d.getMeanXDeviceThree());
		instance.setValue(i++, frame4d.getMeanYDeviceThree());
		instance.setValue(i++, frame4d.getMeanZDeviceThree());
		instance.setValue(i++, frame4d.getVarXDeviceThree());
		instance.setValue(i++, frame4d.getVarYDeviceThree());
		instance.setValue(i++, frame4d.getVarZDeviceThree());
		instance.setValue(i++, frame4d.getCorXYDeviceThree());
		instance.setValue(i++, frame4d.getCorYZDeviceThree());
		instance.setValue(i++, frame4d.getCorZXDeviceThree());

		// device four
		instance.setValue(i++, frame4d.getMeanXDeviceFour());
		instance.setValue(i++, frame4d.getMeanYDeviceFour());
		instance.setValue(i++, frame4d.getMeanZDeviceFour());
		instance.setValue(i++, frame4d.getVarXDeviceFour());
		instance.setValue(i++, frame4d.getVarYDeviceFour());
		instance.setValue(i++, frame4d.getVarZDeviceFour());
		instance.setValue(i++, frame4d.getCorXYDeviceFour());
		instance.setValue(i++, frame4d.getCorYZDeviceFour());
		instance.setValue(i++, frame4d.getCorZXDeviceFour());

		return instance;
	}
}

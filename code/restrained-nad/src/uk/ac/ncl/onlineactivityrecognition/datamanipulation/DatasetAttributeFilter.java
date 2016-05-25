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

import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;

public class DatasetAttributeFilter {

	public static Instances getLowDimDataset(Instances dataset, String[] activityLabels) {
		FastVector schema = new FastVector();

		// foot
		schema.addElement(new Attribute("meanZdeviceOne"));

		// upper leg
		schema.addElement(new Attribute("meanZdeviceFour"));
		schema.addElement(new Attribute("varZdeviceFour"));

		// lower leg
		schema.addElement(new Attribute("meanXdeviceThree"));

		// chest
		schema.addElement(new Attribute("meanXdeviceTwo"));
		schema.addElement(new Attribute("varXdeviceTwo"));
		schema.addElement(new Attribute("meanZdeviceTwo"));
		schema.addElement(new Attribute("varZdeviceTwo"));
		
		// activity
		FastVector activities = new FastVector(activityLabels.length);
		for (int i = 0; i < activityLabels.length; i++) {
			activities.addElement(activityLabels[i]);
		}

		Attribute activityAtt = new Attribute("activity", activities);
		schema.addElement(activityAtt);

		Instances insts = new Instances("low_dim", schema, dataset.numInstances());
		insts.setClassIndex(insts.numAttributes() - 1);

		for (int i = 0; i < dataset.numInstances(); i++) {
			Instance bigInst = dataset.instance(i);
			Instance smallInst = new Instance(schema.size());
			smallInst.setDataset(insts);

			// foot
			smallInst.setValue(0, bigInst.value(2));

			// upper leg
			smallInst.setValue(1, bigInst.value(29));
			smallInst.setValue(2, bigInst.value(32));

			// lower leg
			smallInst.setValue(3, bigInst.value(18));

			//chest
			smallInst.setValue(4, bigInst.value(9));
			smallInst.setValue(5, bigInst.value(12));			
			smallInst.setValue(6, bigInst.value(11));
			smallInst.setValue(7, bigInst.value(14));

			smallInst.setClassValue(bigInst.classValue());

			insts.add(smallInst);
		}

		return insts;
	}

	public static Instance getLowDimDataset(Instance bigInst, Instances smallDataset) {
		Instance smallInst = new Instance(smallDataset.numAttributes());		
		smallInst.setDataset(smallDataset);

		// foot
		smallInst.setValue(0, bigInst.value(2));

		// upper leg
		smallInst.setValue(1, bigInst.value(29));
		smallInst.setValue(2, bigInst.value(32));

		// lower leg
		smallInst.setValue(3, bigInst.value(18));

		//chest
		smallInst.setValue(4, bigInst.value(9));
		smallInst.setValue(5, bigInst.value(12));			
		smallInst.setValue(6, bigInst.value(11));
		smallInst.setValue(7, bigInst.value(14));

		smallInst.setClassValue(bigInst.classValue());

		return smallInst;
	}

}

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
 
package uk.ac.ncl.onlineactivityrecognition.db;

import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;
import java.nio.ByteBuffer;
import java.util.LinkedList;
import java.util.zip.ZipOutputStream;

import uk.ac.ncl.onlineactivityrecognition.datamanipulation.AccelerometerSample;
import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.util.Log;

public class InstancesDataSource implements Serializable {

	private static final long serialVersionUID = 7501952001097207135L;

	// DB fields
	private SQLiteDatabase db;
	private ARSQLiteHelper helper;

	private String[] INSTANCES_COLUMN_NAMES = {
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_X_DEVICE_ONE, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Y_DEVICE_ONE, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Z_DEVICE_ONE,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_X_DEVICE_ONE, 
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Y_DEVICE_ONE,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Z_DEVICE_ONE,
			ARSQLiteHelper.INSTANCES_COLUMN_COR_XY_DEVICE_ONE, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_YZ_DEVICE_ONE, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_ZX_DEVICE_ONE,

			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_X_DEVICE_TWO, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Y_DEVICE_TWO, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Z_DEVICE_TWO,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_X_DEVICE_TWO, 
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Y_DEVICE_TWO,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Z_DEVICE_TWO,
			ARSQLiteHelper.INSTANCES_COLUMN_COR_XY_DEVICE_TWO, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_YZ_DEVICE_TWO, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_ZX_DEVICE_TWO,

			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_X_DEVICE_THREE, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Y_DEVICE_THREE, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Z_DEVICE_THREE,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_X_DEVICE_THREE, 
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Y_DEVICE_THREE,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Z_DEVICE_THREE,
			ARSQLiteHelper.INSTANCES_COLUMN_COR_XY_DEVICE_THREE, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_YZ_DEVICE_THREE, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_ZX_DEVICE_THREE,

			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_X_DEVICE_FOUR, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Y_DEVICE_FOUR, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Z_DEVICE_FOUR,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_X_DEVICE_FOUR, 
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Y_DEVICE_FOUR,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Z_DEVICE_FOUR,
			ARSQLiteHelper.INSTANCES_COLUMN_COR_XY_DEVICE_FOUR, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_YZ_DEVICE_FOUR, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_ZX_DEVICE_FOUR,

			ARSQLiteHelper.INSTANCES_COLUMN_ACTIVITY,

			ARSQLiteHelper.INSTANCES_COLUMN_SEGMENT_NUMBER,
			ARSQLiteHelper.INSTANCES_COLUMN_T_START,
			ARSQLiteHelper.INSTANCES_COLUMN_T_END,

			ARSQLiteHelper.INSTANCES_COLUMN_ID // goes last, for convenience
	};

	private String[] UNLABELLED_INSTANCES_COLUMN_NAMES = {
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_X_DEVICE_ONE, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Y_DEVICE_ONE, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Z_DEVICE_ONE,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_X_DEVICE_ONE, 
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Y_DEVICE_ONE,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Z_DEVICE_ONE,
			ARSQLiteHelper.INSTANCES_COLUMN_COR_XY_DEVICE_ONE, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_YZ_DEVICE_ONE, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_ZX_DEVICE_ONE,

			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_X_DEVICE_TWO, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Y_DEVICE_TWO, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Z_DEVICE_TWO,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_X_DEVICE_TWO, 
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Y_DEVICE_TWO,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Z_DEVICE_TWO,
			ARSQLiteHelper.INSTANCES_COLUMN_COR_XY_DEVICE_TWO, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_YZ_DEVICE_TWO, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_ZX_DEVICE_TWO,

			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_X_DEVICE_THREE, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Y_DEVICE_THREE, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Z_DEVICE_THREE,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_X_DEVICE_THREE, 
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Y_DEVICE_THREE,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Z_DEVICE_THREE,
			ARSQLiteHelper.INSTANCES_COLUMN_COR_XY_DEVICE_THREE, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_YZ_DEVICE_THREE, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_ZX_DEVICE_THREE,

			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_X_DEVICE_FOUR, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Y_DEVICE_FOUR, 
			ARSQLiteHelper.INSTANCES_COLUMN_MEAN_Z_DEVICE_FOUR,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_X_DEVICE_FOUR, 
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Y_DEVICE_FOUR,
			ARSQLiteHelper.INSTANCES_COLUMN_VAR_Z_DEVICE_FOUR,
			ARSQLiteHelper.INSTANCES_COLUMN_COR_XY_DEVICE_FOUR, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_YZ_DEVICE_FOUR, 
			ARSQLiteHelper.INSTANCES_COLUMN_COR_ZX_DEVICE_FOUR,

			ARSQLiteHelper.TIMESTAMP_COLUMN,
			ARSQLiteHelper.SMALL_CONFIDENCE_COLUMN,
			ARSQLiteHelper.BIG_CONFIDENCE_COLUMN,
			
			ARSQLiteHelper.INSTANCES_COLUMN_ID
	};

	String dirPath;

	File fileInstances;
	FileOutputStream fosInstances;

	File fileReadingsDeviceOne;
	FileOutputStream fosReadingsDeviceOne;
	long lastTimestampDeviceOne;

	File fileReadingsDeviceTwo;
	FileOutputStream fosReadingsDeviceTwo;
	long lastTimestampDeviceTwo;

	File fileReadingsDeviceThree;
	FileOutputStream fosReadingsDeviceThree;
	long lastTimestampDeviceThree;

	File fileReadingsDeviceFour;
	FileOutputStream fosReadingsDeviceFour;
	long lastTimestampDeviceFour;
	
	File fileReadingsOnboard;
	FileOutputStream fosReadingsOnboard;
	long lastTimestampOnboard;

	final static byte separatorByte = ' ';


	/*
	private String[] READINGS_DEVICE_ONE_COLUMN_NAMES = {
			ARSQLiteHelper.READINGS_COLUMN_T,
			ARSQLiteHelper.READINGS_COLUMN_X,
			ARSQLiteHelper.READINGS_COLUMN_Y,
			ARSQLiteHelper.READINGS_COLUMN_Z,
			ARSQLiteHelper.READINGS_COLUMN_ID
	};

	private String[] READINGS_DEVICE_TWO_COLUMN_NAMES = {
			ARSQLiteHelper.READINGS_COLUMN_T,
			ARSQLiteHelper.READINGS_COLUMN_X,
			ARSQLiteHelper.READINGS_COLUMN_Y,
			ARSQLiteHelper.READINGS_COLUMN_Z,
			ARSQLiteHelper.READINGS_COLUMN_ID
	};

	private String[] READINGS_DEVICE_THREE_COLUMN_NAMES = {
			ARSQLiteHelper.READINGS_COLUMN_T,
			ARSQLiteHelper.READINGS_COLUMN_X,
			ARSQLiteHelper.READINGS_COLUMN_Y,
			ARSQLiteHelper.READINGS_COLUMN_Z,
			ARSQLiteHelper.READINGS_COLUMN_ID
	};

	private String[] READINGS_DEVICE_FOUR_COLUMN_NAMES = {
			ARSQLiteHelper.READINGS_COLUMN_T,
			ARSQLiteHelper.READINGS_COLUMN_X,
			ARSQLiteHelper.READINGS_COLUMN_Y,
			ARSQLiteHelper.READINGS_COLUMN_Z,
			ARSQLiteHelper.READINGS_COLUMN_ID
	};
	 */

	private String[] READINGS_COLUMN_NAMES = {
			ARSQLiteHelper.READINGS_COLUMN_T,
			ARSQLiteHelper.READINGS_COLUMN_X,
			ARSQLiteHelper.READINGS_COLUMN_Y,
			ARSQLiteHelper.READINGS_COLUMN_Z,
			ARSQLiteHelper.READINGS_COLUMN_ID
	};

	private FastVector schema;
	int numWekaAtts = 37; // 4x9 features + 1 label

	public InstancesDataSource(Context context, FastVector schema) {
		helper = new ARSQLiteHelper(context);
		this.schema = schema;

		//dirPath = context.getFilesDir().getPath().toString();
	}

	public void open() {
		if (db == null) {
			db = helper.getWritableDatabase();
		}

		try {
			//openInstancesStream();
			openReadingsStream();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}


	private void openInstancesStream() throws IOException {
		if (fileInstances == null || !fileInstances.exists()) {
			fileInstances = new File(dirPath + "/instances.bytes");	
			if (!fileInstances.exists()) {
				fileInstances.createNewFile();
				Log.i("Writing bytes", "fileInstances created");
			}
			fosInstances = new FileOutputStream(fileInstances, true);
		}

	}

	public void setFilesDir(String dir) {
		dirPath = dir;
	}

	private void openReadingsStream() throws IOException {
		Log.i("DB", "openReadingsStream()");
		// device one
		if (fileReadingsDeviceOne == null || !fileReadingsDeviceOne.exists()) {
			fileReadingsDeviceOne = new File(dirPath + "/readings_device_one.bytes");
			if (!fileReadingsDeviceOne.exists()) {
				fileReadingsDeviceOne.createNewFile();
				Log.i("Writing bytes", "fileReadings created");
			}
			fosReadingsDeviceOne = new FileOutputStream(fileReadingsDeviceOne, true);
		}

		// device two
		if (fileReadingsDeviceTwo == null || !fileReadingsDeviceTwo.exists()) {
			fileReadingsDeviceTwo = new File(dirPath + "/readings_device_two.bytes");
			if (!fileReadingsDeviceTwo.exists()) {
				fileReadingsDeviceTwo.createNewFile();
				Log.i("Writing bytes", "fileReadings created");
			}
			fosReadingsDeviceTwo = new FileOutputStream(fileReadingsDeviceTwo, true);
		}

		// device three
		if (fileReadingsDeviceThree == null || !fileReadingsDeviceThree.exists()) {
			fileReadingsDeviceThree = new File(dirPath + "/readings_device_three.bytes");
			if (!fileReadingsDeviceThree.exists()) {
				fileReadingsDeviceThree.createNewFile();
				Log.i("Writing bytes", "fileReadings created");
			}
			fosReadingsDeviceThree = new FileOutputStream(fileReadingsDeviceThree, true);
		}

		// device four
		if (fileReadingsDeviceFour == null || !fileReadingsDeviceFour.exists()) {
			fileReadingsDeviceFour = new File(dirPath + "/readings_device_four.bytes");
			if (!fileReadingsDeviceFour.exists()) {
				fileReadingsDeviceFour.createNewFile();
				Log.i("Writing bytes", "fileReadings created");
			}
			fosReadingsDeviceFour = new FileOutputStream(fileReadingsDeviceFour, true);
		}
		
		// onboard 
		if (fileReadingsOnboard == null || !fileReadingsOnboard.exists()) {
			fileReadingsOnboard = new File(dirPath + "/readings_onboard.bytes");
			if (!fileReadingsOnboard.exists()) {
				fileReadingsOnboard.createNewFile();
				Log.i("Writing bytes", "fileReadings created");
			}
			fosReadingsOnboard = new FileOutputStream(fileReadingsOnboard, true);
		}
	}

	private byte[] readingToBytes(AccelerometerSample sample) throws IOException {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		DataOutputStream dos = new DataOutputStream(baos);
		dos.writeLong(sample.getT());
		dos.writeDouble(sample.getX());
		dos.writeDouble(sample.getY());
		dos.writeDouble(sample.getZ());
		dos.flush();
		dos.close();
		return baos.toByteArray();
	}

	/*
	private byte[] instanceToBytes(Instance instance, int segmentNumber, long firstTimestamp, long lastTimestamp) 
			throws IOException {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		DataOutputStream dos = new DataOutputStream(baos);
		for (int i = 0; i < instance.numAttributes(); i++) {
			dos.writeDouble(instance.value(i));
			// the activity is a int, but stored as a double
		}
		dos.writeInt(segmentNumber);
		dos.writeLong(firstTimestamp);
		dos.writeLong(lastTimestamp);
		dos.flush();
		dos.close();
		return baos.toByteArray();
	}

	public void appendInstanceBytes(Instance inst, int segmentNumber, long firstTimestamp, long lastTimestamp) 
			throws IOException {
		byte[] instanceBytes = instanceToBytes(inst, segmentNumber, firstTimestamp, lastTimestamp);
		fosInstances.write(instanceBytes);
		fosInstances.write(separatorByte);
		fosInstances.flush();
	}
	 */

	public void appendReadingBytes(AccelerometerSample sample, int deviceNumber) {
		//Log.i("InstancesDataSource", "appendReadingBytes(" + deviceNumber + ")");
		FileOutputStream fosReadings = null;
		long sampleTimestamp = sample.getT();
		switch(deviceNumber) {
		case 1:
			fosReadings = fosReadingsDeviceOne;
			if (lastTimestampDeviceOne < sampleTimestamp) {
				lastTimestampDeviceOne = sampleTimestamp;
			} else {
				return;
			}
			break;
		case 2:
			fosReadings = fosReadingsDeviceTwo;
			if (lastTimestampDeviceTwo < sampleTimestamp) {
				lastTimestampDeviceTwo = sampleTimestamp;
			} else {
				return;
			}
			break;
		case 3:
			fosReadings = fosReadingsDeviceThree;
			if (lastTimestampDeviceThree < sampleTimestamp) {
				lastTimestampDeviceThree = sampleTimestamp;
			} else {
				return;
			}
			break;
		case 4:
			fosReadings = fosReadingsDeviceFour;
			if (lastTimestampDeviceFour < sampleTimestamp) {
				lastTimestampDeviceFour = sampleTimestamp;
			} else {
				return;
			}
			break;
		default:
			Log.w("Flushing Device Readings", "Bad device number " + deviceNumber);
			break;
		}
		try {
			byte[] readingBytes = readingToBytes(sample);
			fosReadings.write(readingBytes);
			fosReadings.write(separatorByte);
			//int numBytes = readingBytes.length + newlineBytes.length;
			//Log.i("Writing bytes", numBytes + " per reading");
			fosReadings.flush();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	public void close() {
		Log.w("DB", "InstancesDataSource.close()");
		helper.close();
		//		try {
		//			fosInstances.flush();
		//			fosInstances.close();
		//
		//			fosReadings.flush();
		//			fosReadings.close();
		//		} catch(IOException e) {
		//			throw new RuntimeException(e);
		//		}
	}

	/*
	private long saveInstance(Instance inst, int segmentNumber, long startTimestamp, long endTimestamp) {
		//try {
		//	this.appendInstanceBytes(inst, segmentNumber, firstTimestamp, lastTimestamp);
		//} catch (IOException e) {
		//	throw new RuntimeException(e);
		//}

		ContentValues values = new ContentValues();
		int i = 0;
		for (; i < inst.numAttributes(); i++) {
			values.put(INSTANCES_COLUMN_NAMES[i], inst.value(i));
		}
		values.put(INSTANCES_COLUMN_NAMES[i++], segmentNumber);
		values.put(INSTANCES_COLUMN_NAMES[i++], startTimestamp);
		values.put(INSTANCES_COLUMN_NAMES[i], endTimestamp);

		long id = db.insert(ARSQLiteHelper.TABLE_INSTANCES, "", values);
		Log.i("DB", "Added instance " + id + " to database");
		Log.i("DB", getNumInstances() + " instances in DB");

		return id;
	}
	 */

	public long saveUnlabelledInstance(Instance inst, long timestamp, double smallConfidence, double bigConfidence) {
		ContentValues values = new ContentValues();

		int i;
		for (i = 0; i < inst.numAttributes() - 1; i++) { // without activity attribute
			values.put(UNLABELLED_INSTANCES_COLUMN_NAMES[i], inst.value(i));
		}
		values.put(UNLABELLED_INSTANCES_COLUMN_NAMES[i++], timestamp);
		values.put(UNLABELLED_INSTANCES_COLUMN_NAMES[i++], smallConfidence);
		values.put(UNLABELLED_INSTANCES_COLUMN_NAMES[i++], bigConfidence);
		

		long id = db.insert(ARSQLiteHelper.TABLE_UNLABELLED_INSTANCES, "", values);
		Log.i("DB", "Added unlabelled instance " + id + " to database");
		Log.i("DB", getNumUnlabelledInstances() + " unlabelled instances in DB");

		return id;
	}

	public long saveInstancePOJO(InstancePOJO pojo) {
		Instance inst = pojo.getInstance();
		int segmentNumber = pojo.getSegmentNumber();
		long firstTimestamp = pojo.getStartTimestamp();
		long lastTimestamp = pojo.getEndTimestamp();

		/*
		int activityLabel = (int) inst.classValue();
		if ((activityLabel >= 0) && (segmentNumber < 0)) {
			String msg = "Invalid data: actiivtyLabel=" + activityLabel + " and segmentNumber=" + segmentNumber;
			throw new RuntimeException(msg);
		}
		*/

		//try {
		//	this.appendInstanceBytes(inst, segmentNumber, firstTimestamp, lastTimestamp);
		//} catch (IOException e) {
		//	throw new RuntimeException(e);
		//}

		ContentValues values = new ContentValues();
		int i = 0;
		for (; i < inst.numAttributes(); i++) {
			values.put(INSTANCES_COLUMN_NAMES[i], inst.value(i));
		}
		values.put(INSTANCES_COLUMN_NAMES[i++], segmentNumber);
		values.put(INSTANCES_COLUMN_NAMES[i++], firstTimestamp);
		values.put(INSTANCES_COLUMN_NAMES[i], lastTimestamp);

		long id = db.insert(ARSQLiteHelper.TABLE_INSTANCES, "", values);
		Log.e("DB", "Added instance " + id + " to database. Class " + pojo.getInstance().classValue());
		Log.i("DB", getNumInstances() + " instances in DB");

		return id;
	}

	public Instances getAllInstances(boolean labelledOnly) {		
		Cursor cursor = db.query(ARSQLiteHelper.TABLE_INSTANCES, 
				INSTANCES_COLUMN_NAMES, null, null, null, null, null);

		Instances dataset = new Instances("ar", schema, 0);
		dataset.setClassIndex(dataset.numAttributes() - 1);
		if (cursor != null) {
			cursor.moveToFirst();
			while (!cursor.isAfterLast()) {
				Instance inst = getInstanceFromCursor(cursor);
				double classValue = inst.value(dataset.numAttributes() - 1);
				if ((labelledOnly && (classValue >= 0)) || !labelledOnly ) {
					dataset.add(inst);
				}
				cursor.moveToNext();
			}
		}

		return dataset;
	}

	public Instances getUnlabelledInstances() {
		Cursor cursor = db.query(ARSQLiteHelper.TABLE_UNLABELLED_INSTANCES, 
				UNLABELLED_INSTANCES_COLUMN_NAMES, null, null, null, null, null);

		Instances dataset = new Instances("ar", schema, 0);
		dataset.setClassIndex(dataset.numAttributes() - 1);
		/*
		if (cursor != null) {
			cursor.moveToFirst();
			while (!cursor.isAfterLast()) {
				Instance inst = getInstanceFromCursor(cursor);
				dataset.add(inst);
				cursor.moveToNext();
			}
		}
		 */
		return dataset;
	}

	public int getNumInstances() {		
		String qText = "SELECT " + ARSQLiteHelper.INSTANCES_COLUMN_ID + 
				" FROM " + ARSQLiteHelper.TABLE_INSTANCES;
		Cursor cursor = db.rawQuery(qText, null);
		return cursor.getCount();
	}

	public int getNumUnlabelledInstances() {
		String qText = "SELECT " + ARSQLiteHelper.INSTANCES_COLUMN_ID + 
				" FROM " + ARSQLiteHelper.TABLE_UNLABELLED_INSTANCES;
		Cursor cursor = db.rawQuery(qText, null);
		return cursor.getCount();	
	}

	public int getNumReadings(int deviceNumber) {
		String tableReadings = null;
		switch (deviceNumber) {
		case 1:
			tableReadings = ARSQLiteHelper.TABLE_READINGS_DEVICE_ONE;
			break;
		case 2:
			tableReadings = ARSQLiteHelper.TABLE_READINGS_DEVICE_TWO;
			break;
		case 3:
			tableReadings = ARSQLiteHelper.TABLE_READINGS_DEVICE_THREE;
			break;
		case 4:
			tableReadings = ARSQLiteHelper.TABLE_READINGS_DEVICE_FOUR;
			break;
		default:
			throw new RuntimeException("Unknown device number " + deviceNumber + " (valid range: 1-4 inclusive)");
		}

		String qText = "SELECT " + ARSQLiteHelper.READINGS_COLUMN_ID + 
				" FROM " + tableReadings;
		Cursor cursor = db.rawQuery(qText, null);
		return cursor.getCount();
	}

	private Instance getInstanceFromCursor(Cursor cursor) {
		Instance inst = new Instance(numWekaAtts);
		for (int i = 0; i < numWekaAtts; i++) {
			inst.setValue(i, cursor.getDouble(i));
		}
		return inst;
	}

	public void deleteAllInstances() {
		Log.i("DB", "Deleting instances. Before deletion: " + getNumInstances() + " instances.");
		db.delete(ARSQLiteHelper.TABLE_INSTANCES, null, null);
		Log.i("DB", "Deleting instances. After deletion : " + getNumInstances() + " instances.");
		//fileInstances.delete();
		//try {
		//	openInstancesStream();
		//	Log.i("DB", "cleared instances file");
		//} catch (IOException e) {
		//	throw new RuntimeException(e);
		//}
	}

	public void deleteAllUnlabelledInstances() {
		Log.i("DB", "Deleting unlabelled instances. Before: " + getNumUnlabelledInstances() + " instances.");
		db.delete(ARSQLiteHelper.TABLE_UNLABELLED_INSTANCES, null, null);
		Log.i("DB", "Deleting unlabelled instances. After :" + getNumUnlabelledInstances() + " instances.");
	}

	public void saveReading(AccelerometerSample sample, int deviceNumber) {
		//		try {
		//			this.appendReadingBytes(sample);
		//		} catch (IOException e) {
		//			throw new RuntimeException(e);
		//		} 

		ContentValues values = new ContentValues();
		int i = 0;
		values.put(READINGS_COLUMN_NAMES[i++], sample.getT());
		values.put(READINGS_COLUMN_NAMES[i++], sample.getX());
		values.put(READINGS_COLUMN_NAMES[i++], sample.getY());
		values.put(READINGS_COLUMN_NAMES[i++], sample.getZ());

		String tableReadings = null;
		switch (deviceNumber) {
		case 1:
			tableReadings = ARSQLiteHelper.TABLE_READINGS_DEVICE_ONE;
			break;
		case 2:
			tableReadings = ARSQLiteHelper.TABLE_READINGS_DEVICE_TWO;
			break;
		case 3:
			tableReadings = ARSQLiteHelper.TABLE_READINGS_DEVICE_THREE;
			break;
		case 4:
			tableReadings = ARSQLiteHelper.TABLE_READINGS_DEVICE_FOUR;
			break;
		default:
			throw new RuntimeException("Unknown device number " + deviceNumber + " (valid range: 1-4 inclusive)");
		}

		db.insert(tableReadings, "", values);
	}

	public void deleteAllReadings() {
		for (int deviceNumber = 1; deviceNumber <= 4; deviceNumber++) {
			Log.i("DB", "Deleting readings. Before: " + getNumReadings(deviceNumber) + " readings.");
			String tableReadings = null;
			switch (deviceNumber) {
			case 1:
				tableReadings = ARSQLiteHelper.TABLE_READINGS_DEVICE_ONE;
				break;
			case 2:
				tableReadings = ARSQLiteHelper.TABLE_READINGS_DEVICE_TWO;
				break;
			case 3:
				tableReadings = ARSQLiteHelper.TABLE_READINGS_DEVICE_THREE;
				break;
			case 4:
				tableReadings = ARSQLiteHelper.TABLE_READINGS_DEVICE_FOUR;
				break;
			default:
				throw new RuntimeException("Unknown device number " + deviceNumber + " (valid range: 1-4 inclusive)");
			}
			db.delete(tableReadings, null, null);
			Log.i("DB", "Deleting readings. After : " + getNumReadings(deviceNumber) + " readings.");
			//		fileReadings.delete();
			//		try {
			//			openReadingsStream();
			//			Log.i("DB", "cleared readings file");
			//		} catch (IOException e) {
			//			throw new RuntimeException(e);
			//		}
		}
	}

	public void vacuum() {
		db.execSQL("vacuum");
	}

	public void saveOnboardSamples(LinkedList<AccelerometerSample> list) {
		// TODO
		for (AccelerometerSample sample : list) {
			try {
				byte[] readingBytes = readingToBytes(sample);
				fosReadingsOnboard.write(readingBytes);
				fosReadingsOnboard.write(separatorByte);
				//int numBytes = readingBytes.length + newlineBytes.length;
				//Log.i("Writing bytes", numBytes + " per reading");
				fosReadingsOnboard.flush();
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}
	
	public int getNumAnnotatedSegments() {
		String qText = "SELECT " + ARSQLiteHelper.INSTANCES_COLUMN_ACTIVITY + 
				", " + ARSQLiteHelper.INSTANCES_COLUMN_SEGMENT_NUMBER +  
				" FROM " + ARSQLiteHelper.TABLE_INSTANCES;
		Cursor cursor = db.rawQuery(qText, null);
		if (!cursor.moveToFirst() ) {
			return 0;
		}
		
		int numAnnotatedSegments = 0;
		int lastSegmentNumber = -4;
		int lastInstanceActivity = - 1; 
		
		while (true) {
			int currentActivity = cursor.getInt(0);
			int currentSegmentNumber = cursor.getInt(1);
			
			if ((currentActivity >= 0) && (currentSegmentNumber >= 0)) {
				if ((currentActivity != lastInstanceActivity) && (currentSegmentNumber != lastSegmentNumber)) {
					++numAnnotatedSegments;
				}
			}
			
			lastInstanceActivity = currentActivity;
			lastSegmentNumber = currentSegmentNumber;
			
			cursor.moveToNext();
			
			if (cursor.isLast()) {
				break;
			}
		}
		
		return numAnnotatedSegments;
	}
}

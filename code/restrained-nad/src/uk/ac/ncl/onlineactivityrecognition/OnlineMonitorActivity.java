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
 
package uk.ac.ncl.onlineactivityrecognition;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.nio.ByteBuffer;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.UUID;

import uk.ac.ncl.onlineactivityrecognition.classification.MyNaiveBayes;
import uk.ac.ncl.onlineactivityrecognition.classification.OnlineBagNaiveBayes;
import uk.ac.ncl.onlineactivityrecognition.datamanipulation.AccelerometerFrame;
import uk.ac.ncl.onlineactivityrecognition.datamanipulation.AccelerometerFrameFourDevices;
import uk.ac.ncl.onlineactivityrecognition.datamanipulation.AccelerometerSample;
import uk.ac.ncl.onlineactivityrecognition.datamanipulation.ChoreographedSlidingWindow;
import uk.ac.ncl.onlineactivityrecognition.datamanipulation.DatasetAttributeFilter;
import uk.ac.ncl.onlineactivityrecognition.datamanipulation.FeatureRangeCalculatorLowMemory;
import uk.ac.ncl.onlineactivityrecognition.datamanipulation.FrameToInstanceConverter;
import uk.ac.ncl.onlineactivityrecognition.datamanipulation.LabelInquiryDecident;
import uk.ac.ncl.onlineactivityrecognition.datamanipulation.LabelInquiryDecident.BetaFunction;
import uk.ac.ncl.onlineactivityrecognition.datamanipulation.LabelInquiryDecident.Metric;
import uk.ac.ncl.onlineactivityrecognition.datamanipulation.SegmentBoundaryDetector;
import uk.ac.ncl.onlineactivityrecognition.db.InstancePOJO;
import uk.ac.ncl.onlineactivityrecognition.db.InstancesDataSource;
import weka.classifiers.lazy.IB1;
import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.converters.ArffLoader;
import weka.core.converters.CSVLoader;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCallback;
import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattDescriptor;
import android.bluetooth.BluetoothGattService;
import android.bluetooth.BluetoothManager;
import android.bluetooth.BluetoothProfile;
import android.content.Context;
import android.content.Intent;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.media.AudioManager;
import android.media.ToneGenerator;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.os.PowerManager;
import android.os.SystemClock;
import android.os.Vibrator;
import android.util.Log;
import android.view.Gravity;
import android.view.Menu;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;
import android.widget.Toast;

public class OnlineMonitorActivity extends Activity implements SensorEventListener {

	/*
	public class PercentileCalculatorTask extends 
	AsyncTask<FeatureRangeCalculator, Void, Void> {

		@Override
		protected void onPreExecute() {
			//stopAccelerometerMonitoring(false);
			makeInteractive.setEnabled(false);
		}	

		@Override
		protected Void doInBackground(FeatureRangeCalculator... frcs) {
			FeatureRangeCalculator frc = frcs[0];
			if (frc.percentilesComputed()) {
				Log.i("Feature Range Calculator", "Ranges already computed. Skipping FRC and NB_CLUSTER computation");
				return null;
			}

			Log.i("Feature Range Calculator", "calculating ranges...");
			frc.calculateRanges();
			Log.i("Feature Range Calculator", "done calculating ranges");

			try {
				// EM
				//unlabelledData.deleteAttributeAt(unlabelledData.numAttributes() - 1);
				Instances unlabelledScaledData = frc.getAllScaledInstances(schema);
				unlabelledScaledData.setClassIndex(unlabelledScaledData.numAttributes() - 1);
				Remove filter = new Remove();
				filter.setAttributeIndices(String.valueOf(unlabelledScaledData.classIndex() + 1));
				filter.setInputFormat(unlabelledScaledData);
				Instances unlabelledDataWithoutClass = Filter.useFilter(unlabelledScaledData, filter);

				frc.dropSignalData();
				frc.dropStatsIntermediaryData();

				Log.i("Unlabelled scaled data", unlabelledScaledData.numInstances() + " unlabelled instances");
				if (unlabelledScaledData.numInstances() > 0) {
					Log.i("Unlabelled scaled data", "Starting clustering EM with " + 
							unlabelledDataWithoutClass.numInstances() + " instances...");
					EM clusterer = new EM();
					clusterer.buildClusterer(unlabelledDataWithoutClass);
					Log.i("Unlabelled scaled data", "Done with clustering EM.");
					numClusters = clusterer.numberOfClusters();
					Log.i("Unlabelled scaled data", numClusters + " clusters.");

					if (numClusters <= 1) {
						numClusters = 2;
						Log.i("Unlabelled scaled data", "Forcing " + numClusters + " clusters");
						clusterer = new EM();
						clusterer.setNumClusters(numClusters);
						clusterer.buildClusterer(unlabelledDataWithoutClass);
					}
					priorClassesCoverage = new int[numClusters];
					Log.i("NB Cluster", "clustersCoverage has " + numClusters + " clusters");

					FastVector clusterClassValues = new FastVector(numClusters);
					for (int i = 0; i < numClusters; i++) {
						clusterClassValues.addElement("cluster_class_" + i);
					}

					boolean clusterActivityExists = false;
					for (int k = 0; k < unlabelledScaledData.numAttributes(); k++) {
						if (unlabelledScaledData.attribute(k).name() == "clusterActivity") {
							clusterActivityExists = true;
						}
					}

					if (!clusterActivityExists) {
						Attribute clusterClassAtt = new Attribute("clusterActivity", clusterClassValues);
						unlabelledScaledData.insertAttributeAt(clusterClassAtt, unlabelledScaledData.numAttributes());
						unlabelledScaledData.setClassIndex(unlabelledScaledData.numAttributes() - 1);
						unlabelledScaledData.deleteAttributeAt(unlabelledScaledData.numAttributes() - 2);
					}

					for (int i = 0; i < unlabelledScaledData.numInstances(); i++) {
						Instance classlessInst = unlabelledDataWithoutClass.instance(i);
						Instance classyInst = unlabelledScaledData.instance(i);
						double clusterClassValue = clusterer.clusterInstance(classlessInst);
						String strClassValue = (String) clusterClassValues.elementAt((int)clusterClassValue);
						classyInst.setClassValue(strClassValue);
					}
					Log.i("Unlabelled scaled data", "Added cluster class values to data");

					priorModel = new NaiveBayes();
					priorModel.buildClassifier(unlabelledScaledData);
					Log.i("Unlabelled scaled data", "Constructed NB model from unlabelled data");

					Log.i("Serialization", "Dropping signal and stats data from FRC");
					frc.dropStatsIntermediaryData();
					frc.dropSignalData();

					FileOutputStream fos;
					ObjectOutputStream oos;

					// persist frc to file
					fos = getApplicationContext().openFileOutput(FILE_NAME_FRC, Context.MODE_PRIVATE);
					oos = new ObjectOutputStream(fos);
					oos.writeObject(frc);
					oos.close();

					// persist clusterDrivenModel to file
					fos = getApplicationContext().openFileOutput(FILE_NAME_NB_CLUSTER, Context.MODE_PRIVATE);
					oos = new ObjectOutputStream(fos);
					oos.writeObject(priorModel);
					oos.close();				

				} else {
					Log.i("Unlabelled scaled data", "Not enough instances to start EM");
					return null;
				}
			} catch (Exception e) {
				//Log.e("", e.getStackTrace());
				//e.printStackTrace();
				//Log.e("", "", e);
				throw new RuntimeException(e);
			}
			return null;
		}

		@Override
		protected void onPostExecute(Void result) {
			makeInteractive.setEnabled(true);
			//startAccelerometerMonitoring(false);
			makeInteractive.setBackgroundColor(
					getResources().getColor(R.color.green));
			makeInteractive.setText("Make Non Interactive \n(Currently Interactive)");
		}
	}
	 */
	int callID = 0;
	/*
	public class ReadingsFlusherTask extends AsyncTask<AccelerometerFrameFourDevices, Void, Void> {

		@Override
		protected Void doInBackground(AccelerometerFrameFourDevices... params) {
			int thisCallID = ++callID;
			AccelerometerFrameFourDevices f4d = params[0];

			AccelerometerFrame[] deviceFrames = new AccelerometerFrame[]{
					f4d.getFrameDeviceOne(), f4d.getFrameDeviceTwo(), 
					f4d.getFrameDeviceThree(), f4d.getFrameDeviceFour()
			};
			for (int devNum = 1; devNum <= 4; devNum++) {
				AccelerometerFrame frame = deviceFrames[devNum - 1];
				int numSamples = frame.getX().length;
				Log.i("DB", "Flushing " + numSamples + " samples for device " + devNum + 
						" [thisCallID = " + thisCallID + "]");
				double[] x = frame.getX();
				double[] y = frame.getY();
				double[] z = frame.getZ();
				long[] t = frame.getT();
				for (int i = 0; i < numSamples; i++) {
					AccelerometerSample sample = new AccelerometerSample(x[i], y[i], z[i], t[i]);
					instDs.saveReading(sample, devNum);
				}
				Log.i("DB", "Done");
			}

			return null;
		}
	}

	public class ReadingsFlusherTask2 extends AsyncTask<List<AccelerometerSample>, Void, Void> {
		int deviceNumber;

		public ReadingsFlusherTask2(int deviceNumber) {
			this.deviceNumber = deviceNumber;
		}

		@Override
		protected Void doInBackground(List<AccelerometerSample>... params) {
			List<AccelerometerSample> lSamples = params[0];
			Log.i("DB", "Flushing " + lSamples.size() + " readings [deviceNumber=" + deviceNumber + "]");
			for (AccelerometerSample sample : lSamples) {
				instDs.saveReading(sample, deviceNumber);
			}
			return null;
		}		
	}

	public class InstancePOJOFlusherTask extends AsyncTask<List<InstancePOJO>, Void, Void> {
		@Override
		protected Void doInBackground(List<InstancePOJO>... params) {
			List<InstancePOJO> lPojos = params[0];
			Log.i("DB", "Flushing " + lPojos.size() + " instance POJOs");
			for (InstancePOJO instancePojo : lPojos) {
				instDs.saveInstancePOJO(instancePojo);
			}
			return null;
		}
	}

	public class UnlabelledInstanceFlusherTask extends AsyncTask<List<Instance>, Void, Void> {

		@Override
		protected Void doInBackground(List<Instance>... params) {
			List<Instance> list = params[0];
			Log.i("DB", "Flushing " + list.size() + " unlabelled instances");
			for (Instance instance : list) {
				instDs.saveUnlabelledInstance(instance);
			}
			return null;
		}

	}
	 */

	TextView txvTimestamp;	
	TextView txvActivity;
	TextView txvConfidence;
	TextView txvBudgetSize;
	TextView txvNumFrames;

	// buttons
	Button btnBell;
	Button pauseResume;
	Button delete;
	Button makeInteractive;

	int segmentCount = 0;

	DecimalFormat df = new DecimalFormat("#.###");

	boolean paused = true;

	boolean physicalActivityDialogueActivated = false;

	boolean useAudibleTone = true;

	boolean isInteractive = false;

	final static String FILE_NAME_FRC = "frc.obj";

	final static int FRC_TAIL_LENGTH = 10;
	FeatureRangeCalculatorLowMemory frc;
	//FeatureRangeCalculator frc;

	final int KERNEL_WIDTH = 3;
	final double THRESHOLD = 0.07;
	final int NUM_IGNORE_SEGMENTS_EITHER_SIDE = 2;
	final int NUM_IGNORE_FIRST_SEGMENTS = 1;
	SegmentBoundaryDetector sbd = new SegmentBoundaryDetector(KERNEL_WIDTH, THRESHOLD, 
			NUM_IGNORE_FIRST_SEGMENTS, NUM_IGNORE_SEGMENTS_EITHER_SIDE);
	final int MAX_SEGMENT_SIZE = 20;

	double smallGamma = 2e-2;
	Metric smallMetric = Metric.AVERAGE_CONFIDENCE_GEOM;
	BetaFunction smallBetaFunction = BetaFunction.LOG_BETA_FUNCTION;

	double bigGamma = 2;
	Metric bigMetric = Metric.AVERAGE_CONFIDENCE;
	BetaFunction bigBetaFunction = BetaFunction.BETA_FUNCTION;
	
	// TODO
	long HEURISTIC_SWITCH_TIMESTAMP;
	final static int SWITCH_OVER_HOUR = 14;

	PowerManager.WakeLock wakeLock;

	// sampling and feature extraction
	final static long WINDOW_DURATION = (long) 5000; // MILIseconds; 5s

	final static int SAMPLING_DELAY_ONBOARD = 10000; // MICROseconds; = 10ms
	LinkedList<AccelerometerSample> listSamplesOnboard = new LinkedList<AccelerometerSample>();

	ChoreographedSlidingWindow slidingWindowDeviceOne;
	ChoreographedSlidingWindow slidingWindowDeviceTwo;
	ChoreographedSlidingWindow slidingWindowDeviceThree;
	ChoreographedSlidingWindow slidingWindowDeviceFour;

	int numFrame;
	AccelerometerFrameFourDevices frame4d;

	// model
	String FILE_NAME_NB_MODEL = "nb.obj";
	//NaiveBayes model;
	//MyNaiveBayes model;
	OnlineBagNaiveBayes model;
	final int BAG_SIZE = 30;
	
	// TODO
	//MyNaiveBayes smallModel;
	IB1 popModel;
	int numUpdates;
	boolean[] popModelCoverage = new boolean[ACTIVITY_LABELS.length];
	final String FILE_NAME_COVERAGE = "popModelCoverage.data";
	int popPredictedClassIndex;
	final String popTrainingFileName = "ncc.arff";	
	String filesDir;
	
	// TODO - remove
	//final int[] CLASS_COVERAGE = new int[ACTIVITY_LABELS.length];
	//boolean allClassesCovered = false;
	

	FastVector schema;
	FastVector unlabelledSchema;

	Attribute attMeanXDeviceOne;
	Attribute attMeanYDeviceOne;
	Attribute attMeanZDeviceOne;
	Attribute attVarXDeviceOne;
	Attribute attVarYDeviceOne;
	Attribute attVarZDeviceOne;
	Attribute attCorXYDeviceOne;
	Attribute attCorYZDeviceOne;
	Attribute attCorZXDeviceOne;

	Attribute attMeanXDeviceTwo;
	Attribute attMeanYDeviceTwo;
	Attribute attMeanZDeviceTwo;
	Attribute attVarXDeviceTwo;
	Attribute attVarYDeviceTwo;
	Attribute attVarZDeviceTwo;
	Attribute attCorXYDeviceTwo;
	Attribute attCorYZDeviceTwo;
	Attribute attCorZXDeviceTwo;

	Attribute attMeanXDeviceThree;
	Attribute attMeanYDeviceThree;
	Attribute attMeanZDeviceThree;
	Attribute attVarXDeviceThree;
	Attribute attVarYDeviceThree;
	Attribute attVarZDeviceThree;
	Attribute attCorXYDeviceThree;
	Attribute attCorYZDeviceThree;
	Attribute attCorZXDeviceThree;

	Attribute attMeanXDeviceFour;
	Attribute attMeanYDeviceFour;
	Attribute attMeanZDeviceFour;
	Attribute attVarXDeviceFour;
	Attribute attVarYDeviceFour;
	Attribute attVarZDeviceFour;
	Attribute attCorXYDeviceFour;
	Attribute attCorYZDeviceFour;
	Attribute attCorZXDeviceFour;

	final static String[] ACTIVITY_LABELS = {
		"walking", /*"running",*/ 
		"sitting", "standing", "sitting knee raises",
		"calf_raises", "squats", 
		"torso_side_to_side", "torso_forward_backward", "torso_twists"};
	final static int NUM_CLASSES = ACTIVITY_LABELS.length;
	Attribute attActivity;

	Instances trainingSet;
	Instances unlabelledData;

	//Instances smallTrainingSet;

	int numInstancesConfidentlyClassified;
	InstancesDataSource instDs;

	final static int BUDGET_SIZE = 500;
	static int budgetSpent;

	final static double MIN_ASK_PROB = 0.2;

	/*
	private void flushListOfSamples(boolean forced) {
		for (int i=1; i <= 4; i++) {
			flushListOfSamplesForDevice(i, forced);
		}
	}
	 */

	/*
	@SuppressWarnings("unchecked")
	private void flushListOfSamplesForDevice(int deviceNumber, boolean forced) {
		if (forced || (listInstancePOJOs.size() > MAX_LIST_INSTANCE_POJOS_SIZE)) {
			List<AccelerometerSample> tempListSamples;
			ReadingsFlusherTask rfTask;

			switch(deviceNumber) {
			case 1:
				tempListSamples = listSamplesDeviceOne;
				listSamplesDeviceOne = new ArrayList<AccelerometerSample>((int)(MAX_LIST_READINGS_SIZE * 1.5));
				rfTask = new ReadingsFlusherTask(deviceNumber);
				rfTask.execute(tempListSamples);
				break;
			case 2:
				tempListSamples = listSamplesDeviceTwo;
				listSamplesDeviceTwo = new ArrayList<AccelerometerSample>((int)(MAX_LIST_READINGS_SIZE * 1.5));
				rfTask = new ReadingsFlusherTask(deviceNumber);
				rfTask.execute(tempListSamples);
				break;
			case 3:
				tempListSamples = listSamplesDeviceThree;
				listSamplesDeviceThree = new ArrayList<AccelerometerSample>((int)(MAX_LIST_READINGS_SIZE * 1.5));
				rfTask = new ReadingsFlusherTask(deviceNumber);
				rfTask.execute(tempListSamples);
				break;
			case 4:
				tempListSamples = listSamplesDeviceFour;
				listSamplesDeviceFour = new ArrayList<AccelerometerSample>((int)(MAX_LIST_READINGS_SIZE * 1.5));
				rfTask = new ReadingsFlusherTask(deviceNumber);
				rfTask.execute(tempListSamples);
				break;
			}
		}
	}
	 */
	/*
	private void flushSamplesInFrame(AccelerometerFrameFourDevices f4d) {
		Log.i("DB", "flushSamplesInFrame()");
		ReadingsFlusherTask rfTask = new ReadingsFlusherTask();
		rfTask.execute(f4d);
	}
	 */

	/*
	@SuppressWarnings("unchecked")
	private void flushListOfInstancePOJOs(List<InstancePOJO> list) {
		if (forced || (listInstancePOJOs.size() > MAX_LIST_INSTANCE_POJOS_SIZE)) {
			List<InstancePOJO> tempListPojos = listInstancePOJOs;
			listInstancePOJOs = new ArrayList<InstancePOJO>(MAX_LIST_INSTANCE_POJOS_SIZE);
			InstancePOJOFlusherTask ipfTask = new InstancePOJOFlusherTask();
			ipfTask.execute(tempListPojos);
		}

	}
	 */

	/*
	@SuppressWarnings("unchecked")
	private void flushUnlabelledInstances() {

		if (forced || (listUnlabelledInstances.size() > MAX_LIST_UNLABELLED_INSTANCES_SIZE)) {
			List<Instance> tempList = listUnlabelledInstances;
			listUnlabelledInstances = new ArrayList<Instance>(MAX_LIST_UNLABELLED_INSTANCES_SIZE);
			UnlabelledInstanceFlusherTask uifTask = new UnlabelledInstanceFlusherTask();
			uifTask.execute(tempList);
		}
	}
	 */

	private void flushUnlabelledFrame(AccelerometerFrameFourDevices f4d, long timestamp, double smallConfidence, double bigConfidence) {
		Log.i("DB flusher", "flushUnlabelledFrame()");
		Message msg = Message.obtain();
		msg.obj = f4d;
		msg.what = MSG_FLUSH_UNLABELLED_FRAME_4_D;
		Bundle bundle = new Bundle();
		bundle.putDouble("smallConfidence", smallConfidence);
		bundle.putDouble("bigConfidence", bigConfidence);
		bundle.putLong("timestamp", timestamp);
		dbFlusherHandler.sendMessage(msg);
	}

	private void flushLabelledPOJO(InstancePOJO pojo) {
		Log.i("DB flusher", "flushLabelledInstance()");
		Message msg = Message.obtain();
		msg.obj = pojo;
		msg.what = MSG_FLUSH_LABELLED_INSTANCE_POJO;
		dbFlusherHandler.sendMessage(msg);
	}

	private void flushOnboardSamples() {
		Log.i("DB flusher", "flushOnboardSamples()");
		Message msg = Message.obtain();
		msg.obj = listSamplesOnboard;
		msg.what = MSG_FLUSH_ONBOARD_SAMPLES;
		dbFlusherHandler.sendMessage(msg);
	}

	@SuppressLint("SimpleDateFormat")
	SimpleDateFormat sdf = new SimpleDateFormat("H:m:s.S");

	Random rand = new Random();

	// start activity request codes
	final static int REQUEST_CODE_USER_ANNOTATION = 1;
	final static int REQUEST_CODE_ENABLE_BLUETOOTH = 2;

	// bluetooth
	private BluetoothAdapter bluetoothAdapter;
	private Handler bluetoothScanHandler; 

	private BluetoothAdapter.LeScanCallback leScanCallback = new BluetoothAdapter.LeScanCallback() {

		@Override
		public void onLeScan(BluetoothDevice device, int rssi, byte[] scanRecord) {
			String devAddr = device.getAddress().toLowerCase();

			Log.i("Bluetooth", "Found device " + devAddr);

			if ((btDeviceOne == null) && devAddr.equals(btAddrOne)) {
				btDeviceOne = device;
				Log.i("Bluetooth", "Found btDeviceOne " + btAddrOne);
			} else if ((btDeviceTwo == null) && devAddr.equals(btAddrTwo)) {
				btDeviceTwo = device;
				Log.i("Bluetooth", "Found btDeviceTwo " + btAddrTwo);				
			} else if ((btDeviceThree == null) && devAddr.equals(btAddrThree)){
				btDeviceThree = device;
				Log.i("Bluetooth", "Found btDeviceThree " + btAddrThree);
			} else if ((btDeviceFour == null) && devAddr.equals(btAddrFour)) {
				btDeviceFour = device;
				Log.i("Bluetooth", "Found btDeviceFour " + btAddrFour);
			} else { 
				if (!devAddr.equals(btAddrOne) && !devAddr.equals(btAddrTwo) && 
						!devAddr.equals(btAddrThree) && !devAddr.equals(btAddrFour)) {
					Log.i("Bluetooth", "Device " + devAddr + " is not in the list of compatible devices.");
					return;
				}
			}

			if ((btDeviceOne != null) && (btDeviceTwo != null) && (btDeviceThree != null) && (btDeviceFour != null)) {
				Log.i("Bluetooth", "Found all devices. Stopping scan.");
				bluetoothAdapter.stopLeScan(leScanCallback);
			}
		}
	};


	boolean gattOneConnected;	
	boolean gattTwoConnected;
	boolean gattThreeConnected;
	boolean gattFourConnected;

	boolean gattOneStreaming;
	boolean gattTwoStreaming;
	boolean gattThreeStreaming;
	boolean gattFourStreaming;

	boolean slidingWindowEnabled;
	Object callBackChoreographySynchronization = new Object();

	public class WAX9GattCallback extends BluetoothGattCallback {

		private int fsmState = 0;

		private void fsmAdvance() {
			fsmState++; 
			Log.i("Bluetooth", "Advancing FSM: fsmState == " + fsmState);
		}
		private void fsmReset() { 
			Log.i("Bluetooth", "Resetting Callback FSM");
			fsmState = 0; 
		}

		@Override
		public void onConnectionStateChange(BluetoothGatt gatt, int status, int newState) {
			Log.i("Bluetooth", "onConnectionStateChange()");
			String deviceAddr = gatt.getDevice().getAddress().toLowerCase();

			synchronized (callBackChoreographySynchronization) {
				if (status == BluetoothGatt.GATT_SUCCESS) {
					if (newState == BluetoothProfile.STATE_CONNECTED) {
						Log.i("Bluetooth", "status == BluetoothGatt.GATT_SUCCESS");
						Log.i("Bluetooth", "STATE_CONNECTED " + deviceAddr);
						fsmReset();
						Log.i("Bluetooth", "Started service discovery for " + deviceAddr);

						if (deviceAddr.equals(btAddrOne)) {
							gattOneConnected = true;
						} else if (deviceAddr.equals(btAddrTwo)) {
							gattTwoConnected = true;
						} else if (deviceAddr.equals(btAddrThree)) {
							gattThreeConnected = true;
						} else if (deviceAddr.equals(btAddrFour)) {
							gattFourConnected = true;
						}

						gatt.discoverServices();

					} else if (newState == BluetoothProfile.STATE_CONNECTING) {
						Log.i("Bluetooth", "STATE_CONNECTING");
					} else if (newState == BluetoothProfile.STATE_DISCONNECTING) {
						Log.i("Bluetooth", "STATE_DISCONNECTING");
					} else if (newState == BluetoothProfile.STATE_DISCONNECTED) {
						Log.i("Bluetooth", "STATE_DISCONNECTED");
						if (deviceAddr.equals(btAddrOne)) {
							gattOneConnected = false;
						} else if (deviceAddr.equals(btAddrTwo)) {
							gattTwoConnected = false;
						} else if (deviceAddr.equals(btAddrThree)) {
							gattThreeConnected = false;
						} else if (deviceAddr.equals(btAddrFour)) {
							gattFourConnected = false;
						}
					}					
				} else {
					Log.i("Bluetooth", "status == " + status + " (" + deviceAddr + ")");

					if (deviceAddr.equals(btAddrOne)) {
						gattOneConnected = false;
						gattOneStreaming = false;
						btHandler.sendMessage(Message.obtain(null, MSG_CONNECT_GATT, btAddrOne));
					} else if (deviceAddr.equals(btAddrTwo)) {
						gattTwoConnected = false;
						gattTwoStreaming = false;
						btHandler.sendMessage(Message.obtain(null, MSG_CONNECT_GATT, btAddrTwo));
					} else if (deviceAddr.equals(btAddrThree)) {
						gattThreeConnected = false;
						gattThreeStreaming = false;
						btHandler.sendMessage(Message.obtain(null, MSG_CONNECT_GATT, btAddrThree));
					} else if (deviceAddr.equals(btAddrFour)) {
						gattFourConnected = false;
						gattFourStreaming = false;
						btHandler.sendMessage(Message.obtain(null, MSG_CONNECT_GATT, btAddrFour));
					}
				}
			} 
		}

		private int accBytesToInt(byte[] bytes, int iStart, int iEnd) {
			return bytes[iEnd] * 0x100 + bytes[iStart];
		}

		@Override
		public void onCharacteristicChanged(BluetoothGatt gatt, BluetoothGattCharacteristic characteristic) {

			if (!slidingWindowEnabled) {
				//Log.i("Bluetooth", "Sliding Window Disabled: " + gattOneConnected + " " + 
				//		gattTwoConnected + " " + gattThreeConnected + " " + gattFourConnected);
				/*
				if (!scanningInProgress) {
					scanLeDevice(true);
				}
				 */
				return;
			}

			long tNow = System.currentTimeMillis();

			//Log.i("Bluetooth", "onCharacteristicChanged()");
			byte[] bytes = characteristic.getValue();
			//int sn = bytesToInt(bytes, 0, 1);

			int x = accBytesToInt(bytes, 2, 3);
			double gx = x / ACCELEROMETER_SCALING_FACTOR;

			int y = accBytesToInt(bytes, 4, 5);
			double gy = y / ACCELEROMETER_SCALING_FACTOR;

			int z = accBytesToInt(bytes, 6, 7);
			double gz = z / ACCELEROMETER_SCALING_FACTOR;

			//Log.i("Bluetooth", "sn=" + sn + " x=" + x + " y=" + y + " z=" + z);
			//Log.i("Bluetooth", gatt.getDevice().getAddress().toLowerCase() + ": sn=" + sn + ": " + 
			//		String.format("x=%.2f, y=%.2f, z=%.2f", gx, gy, gz));
			//Log.i("Bluetooth", gatt.getDevice().getAddress().toLowerCase() + ": sn=" + sn + ": " + 
			//		String.format("x=%d, y=%d, z=%d", x, y, z));
			//Log.i("Bluetooth", "sn=" + sn + " skipped=" + skipped);

			AccelerometerSample reading = new AccelerometerSample(gx, gy, gz, tNow);
			int msgCode = 0;
			String devAddr = gatt.getDevice().getAddress().toLowerCase();
			if (devAddr.equals(btAddrOne)) {
				msgCode = MSG_NEW_READING_DEVICE_ONE;
			} else if (devAddr.equals(btAddrTwo)) {
				msgCode = MSG_NEW_READING_DEVICE_TWO;
			} else if (devAddr.equals(btAddrThree)) {
				msgCode = MSG_NEW_READING_DEVICE_THREE;
			} else if (devAddr.equals(btAddrFour)) {
				msgCode = MSG_NEW_READING_DEVICE_FOUR;
			}

			//Log.i("Bluetooth", "Notification message dispatched. msg.what = " + msgCode);
			btHandler.dispatchMessage(Message.obtain(null, msgCode, reading));
		}

		@Override
		public void onCharacteristicRead(BluetoothGatt gatt, BluetoothGattCharacteristic characteristic, int status) {
			Log.i("Bluetooth", "onCharacteristicRead()");
			byte value = ByteBuffer.wrap(characteristic.getValue()).get();
			Log.i("Bluetooth", "value from characteristic = " + value);
		}

		@Override
		public void onCharacteristicWrite(BluetoothGatt gatt, BluetoothGattCharacteristic characteristic, int status) {
			Log.i("Bluetooth", "onCharacteristicWrite()");
			Log.i("Bluetooth", "  advancing FSM");
			fsmAdvance();
			setupSensor(gatt);
		}

		@Override
		public void onServicesDiscovered(BluetoothGatt gatt, int status) {
			Log.i("Bluetooth", "onServicesDiscovered()");
			if (status == BluetoothGatt.GATT_SUCCESS) {
				/*
				String btAddr = gatt.getDevice().getAddress().toLowerCase();
				List<BluetoothGattService> listServices = gatt.getServices();
				Log.i("Bluetooth", "List of services for " + btAddr + ":");
				for (BluetoothGattService gattService : listServices) {
					Log.i("Bluetooth", "  " + gattService.getUuid() + ":");
					for (BluetoothGattCharacteristic characteristic : gattService.getCharacteristics()) {
						Log.i("Bluetooth" , "    " + characteristic.getUuid() + "(" + characteristic.getPermissions() + ")");

						for (BluetoothGattDescriptor descriptor : characteristic.getDescriptors()) {
							Log.i("Bluetooth", "      " + descriptor.getUuid() + "(" + descriptor.getPermissions() + ")");
						}
					}
				}
				 */
				setupSensor(gatt);
			}
		}

		@Override
		public void onDescriptorWrite(BluetoothGatt gatt, BluetoothGattDescriptor descriptor, int status) {
			Log.i("Bluetooth", "onDescriptorWrite()");
			fsmAdvance();
			setupSensor(gatt);
		}

		private void setupSensor(BluetoothGatt gatt) {
			BluetoothGattService bgs;
			BluetoothGattCharacteristic bgc;
			BluetoothGattDescriptor bgd;
			byte[] value;

			switch (fsmState) {
			case 0:
				// enable notifications on serial port
				bgs = gatt.getService(UUID_SERIAL_PORT_SERVICE);
				bgc = bgs.getCharacteristic(UUID_SERIAL_DATA_OUTPUT);
				bgd = bgc.getDescriptor(UUID_CLIENT_CHARACTERISTIC_CONFIGURATION);
				//value = ByteBuffer.allocate(2).put((byte)1).array();
				//value = new byte[] {0x00, 0x01};
				value = new byte[] {0x01, 0x00};

				gatt.setCharacteristicNotification(bgc, true);	

				//bgd.setValue(value);
				bgd.setValue(BluetoothGattDescriptor.ENABLE_NOTIFICATION_VALUE);
				if (gatt.writeDescriptor(bgd)) {
					Log.i("Bluetooth", "  Successfully attempting to subscribe to serial port notifications");
				}
				break;

			case 1:
				// enable notifications on serial data output
				//bgs = gatt.getService(UUID_SERIAL_PORT_SERVICE);
				//bgc = bgs.getCharacteristic(UUID_SERIAL_DATA_OUTPUT);

				bgs = gatt.getService(UUID_SENSOR_DATA_AND_COMMAND_SERVICE);
				bgc = bgs.getCharacteristic(UUID_SENSOR_DATA);
				bgd = bgc.getDescriptor(UUID_CLIENT_CHARACTERISTIC_CONFIGURATION);
				//value = ByteBuffer.allocate(2).put((byte)1).array();
				value = new byte[] {0x01, 0x00};

				gatt.setCharacteristicNotification(bgc, true);	

				//bgd.setValue(value);
				bgd.setValue(BluetoothGattDescriptor.ENABLE_NOTIFICATION_VALUE);
				if (gatt.writeDescriptor(bgd)) {
					Log.i("Bluetooth", "  Successfully attempting to subscribe to sensor data notifications");
				}
				break;

			case 2:
				// set bluetooth sample rate				
				bgs = gatt.getService(UUID_SERVICE_SETTINGS_AND_STATUS_SERVICE);
				bgc = bgs.getCharacteristic(UUID_SAMPLE_RATE);
				//value = ByteBuffer.allocate(2).put((byte)10).array(); // 10 Hz
				value = BLUETOOTH_NOTIFICATION_RATE;
				bgc.setValue(value);
				if (gatt.writeCharacteristic(bgc)) {
					Log.i("Bluetooth", "  Successfully attempting to set BT sample rate");
				}
				//fsmAdvance();
				//setupSensor(gatt);
				break;

			case 3:
				// set accelerometer on
				bgs = gatt.getService(UUID_SERVICE_SETTINGS_AND_STATUS_SERVICE);
				bgc = bgs.getCharacteristic(UUID_ACCELERATION_ON);
				//value = ByteBuffer.allocate(2).put((byte)1).array();
				value = new byte[] {0x01, 0x00};

				bgc.setValue(value);
				if (gatt.writeCharacteristic(bgc)) {
					Log.i("Bluetooth", "  Successfully attempting to turn on the accelerometer");
				}
				break;

			case 4:
				// set accelerometer sample rate
				bgs = gatt.getService(UUID_SERVICE_SETTINGS_AND_STATUS_SERVICE);
				bgc = bgs.getCharacteristic(UUID_ACCELERATION_RATE);
				//value = ByteBuffer.allocate(2).put((byte)100).array(); // 100 Hz
				value = ACCELEROMETER_RATE;

				bgc.setValue(value);
				if (gatt.writeCharacteristic(bgc)) {
					Log.i("Bluetooth", "  Successfully attempting to set the accelerometer sample rate");
				}
				break;

			case 5:
				// set accelerometer range
				bgs = gatt.getService(UUID_SERVICE_SETTINGS_AND_STATUS_SERVICE);
				bgc = bgs.getCharacteristic(UUID_ACCELERATION_RANGE);
				//value = ByteBuffer.allocate(2).put((byte)4).array();
				value = ACCELEROMETER_RANGE;

				bgc.setValue(value);
				if (gatt.writeCharacteristic(bgc)) {
					Log.i("Bluetooth", "  Successfully attempting to set the accelerometer range");
				}
				break;

			case 6:
				// turn off gyroscope
				bgs = gatt.getService(UUID_SERVICE_SETTINGS_AND_STATUS_SERVICE);
				bgc = bgs.getCharacteristic(UUID_GYRO_OFF);
				//value = ByteBuffer.allocate(2).put((byte)0).array();
				value = new byte[] {0x00, 0x00};

				bgc.setValue(value);
				if (gatt.writeCharacteristic(bgc)) {
					Log.i("Bluetooth", "  Successfully attempting to turn the gyro OFF");
				}
				break;

			case 7:
				// turn off magnetometer
				bgs = gatt.getService(UUID_SERVICE_SETTINGS_AND_STATUS_SERVICE);
				bgc = bgs.getCharacteristic(UUID_MAGN_OFF);
				//value = ByteBuffer.allocate(2).put((byte)0).array();
				value = new byte[] {0x00, 0x00};

				bgc.setValue(value);
				if (gatt.writeCharacteristic(bgc)) {
					Log.i("Bluetooth", "  Successfully attempting to set the magnetometer OFF");
				}
				break;

			case 8:
				// latch and go
				bgs = gatt.getService(UUID_SENSOR_DATA_AND_COMMAND_SERVICE);
				bgc = bgs.getCharacteristic(UUID_ENUMERATED_COMMAND_INPUT);
				//value = ByteBuffer.allocate(2).put((byte)1).array();
				value = new byte[] {0x01, 0x00};

				//				if (gatt.getDevice().getAddress().toLowerCase().equals(btAddrTwo)) {
				//					while(!gattOneConnected) ;
				//				} else if (gatt.getDevice().getAddress().toLowerCase().equals(btAddrThree)) {
				//					while(!gattTwoConnected) ;
				//				} else if (gatt.getDevice().getAddress().toLowerCase().equals(btAddrFour)) {
				//					while(!gattThreeConnected) ;
				//				}

				bgc.setValue(value);
				if (gatt.writeCharacteristic(bgc)) {
					Log.i("Bluetooth", "  Successfully attempting to latch");
				}

				//				if (gatt.getDevice().getAddress().toLowerCase().equals(btAddrOne)) {
				//					gattOneConnected = true;
				//				} else if (gatt.getDevice().getAddress().toLowerCase().equals(btAddrTwo)) {
				//					gattTwoConnected = true;
				//				} else if (gatt.getDevice().getAddress().toLowerCase().equals(btAddrThree)) {
				//					gattThreeConnected = true;
				//				} else if (gatt.getDevice().getAddress().toLowerCase().equals(btAddrFour)) {
				//					gattFourConnected = true;
				//				}

				break;	
			case 9:
				bgs = gatt.getService(UUID_SERVICE_SETTINGS_AND_STATUS_SERVICE);
				bgc = bgs.getCharacteristic(UUID_LED);
				if (gatt.readCharacteristic(bgc)) {
					Log.i("Bluetooth", "  Successfully attempting to read the LED status");
				}
				break;
			}
		}

	}

	// bluetooth sensor positions
	// String addrRightWrist = 
	// Bluetoothdevice btdRightWrist = 

	String btAddrOne = "00:17:e9:61:56:78".toLowerCase();
	BluetoothDevice btDeviceOne; // foot
	BluetoothGatt deviceOneGatt;
	WAX9GattCallback deviceOneCallback = new WAX9GattCallback();

	String btAddrTwo = "00:17:e9:61:57:00".toLowerCase();
	BluetoothDevice btDeviceTwo; // chest
	BluetoothGatt deviceTwoGatt;
	WAX9GattCallback deviceTwoCallback = new WAX9GattCallback();

	String btAddrThree = "00:17:e9:61:56:50".toLowerCase();
	BluetoothDevice btDeviceThree; // lower leg
	BluetoothGatt deviceThreeGatt;
	WAX9GattCallback deviceThreeCallback = new WAX9GattCallback();

	String btAddrFour = "00:17:e9:66:ca:bb".toLowerCase();
	BluetoothDevice btDeviceFour; // upper leg
	BluetoothGatt deviceFourGatt;
	WAX9GattCallback deviceFourCallback = new WAX9GattCallback();

	private static byte[] intToBytes(int value, int numBytes) {
		byte[] bytes = ByteBuffer.allocate(4).putInt(value).array();
		//Collections.reverse(Arrays.asList(bytes));
		bytes = Arrays.copyOfRange(bytes, bytes.length-numBytes, bytes.length);
		// reverse order of bytes
		for (int i=0; i < bytes.length/2; i++) {
			byte temp = bytes[i];
			bytes[i] = bytes[bytes.length-1 - i];
			bytes[bytes.length-1 - i] = temp;
		}
		return bytes;
	}

	final static byte[] ACCELEROMETER_RANGE = {8, 0};
	final static double ACCELEROMETER_SCALING_FACTOR = 32768 / ACCELEROMETER_RANGE[0];
	final static byte[] ACCELEROMETER_RATE = intToBytes(100, 2);
	//final static byte[] BLUETOOTH_NOTIFICATION_RATE = intToBytes(80, 2);
	final static byte[] BLUETOOTH_NOTIFICATION_RATE = intToBytes(60, 2);

	private final static int MSG_CONNECT_GATT = 1000;

	private final static int MSG_NEW_READING_DEVICE_ONE = 2001;
	private final static int MSG_NEW_READING_DEVICE_TWO = 2002;
	private final static int MSG_NEW_READING_DEVICE_THREE = 2003;
	private final static int MSG_NEW_READING_DEVICE_FOUR = 2004;

	Handler btHandler = new Handler() {

		@Override
		public void handleMessage(Message msg) {
			//Log.i("btHandler", "Handling message...");
			switch (msg.what) {
			case MSG_CONNECT_GATT:
				String btAddr = ((String) msg.obj).toLowerCase();
				Log.i("Bluetooth", "Connecting to device " + btAddr);

				if (btAddr.equals(btAddrOne)) {
					deviceOneGatt = btDeviceOne.connectGatt(getApplicationContext(),  true, deviceOneCallback);
					//Log.i("Bluetooth", "btDeviceOne.connectGatt() called");
				} else if (btAddr.equals(btAddrTwo)) {
					//while (!gattOneConnected) ;
					deviceTwoGatt = btDeviceTwo.connectGatt(getApplicationContext(),  true, deviceTwoCallback);
					Log.i("Bluetooth", "btDeviceTwo.connectGatt() called");
				} else if (btAddr.equals(btAddrThree)) {
					//while (!gattTwoConnected) ;
					deviceThreeGatt = btDeviceThree.connectGatt(getApplicationContext(), true, deviceThreeCallback);
					Log.i("Bluetooth", "btDeviceThree.connectGatt() called");
				} else if (btAddr.equals(btAddrFour)) {
					//while (!gattThreeConnected) ;
					deviceFourGatt = btDeviceFour.connectGatt(getApplicationContext(), true, deviceFourCallback);
					Log.i("Bluetooth", "btDeviceFour.connectGatt() called");
				} else {
					Log.i("Bluetooth", "Attempting to connect to " + btAddr + ", but device is not recognized");
				}
				break;

			case MSG_NEW_READING_DEVICE_ONE:
				//Log.i("Bluetooth", "Notification device one");
				gattOneStreaming = true;
				if (gattOneStreaming && gattTwoStreaming && gattThreeStreaming && gattFourStreaming && slidingWindowEnabled) {
					if (slidingWindowDeviceOne == null) {
						long timestamp = System.currentTimeMillis();

						slidingWindowDeviceOne = new ChoreographedSlidingWindow(timestamp, WINDOW_DURATION);
						slidingWindowDeviceTwo = new ChoreographedSlidingWindow(timestamp, WINDOW_DURATION);
						slidingWindowDeviceThree = new ChoreographedSlidingWindow(timestamp, WINDOW_DURATION);
						slidingWindowDeviceFour = new ChoreographedSlidingWindow(timestamp, WINDOW_DURATION);

						Log.i("Sliding window", "Started choreographed sliding windows");
					}
					AccelerometerSample reading = (AccelerometerSample) msg.obj; 
					slidingWindowDeviceOne.add(reading);
					//listSamplesDeviceOne.add(reading);
				}
				break;

			case MSG_NEW_READING_DEVICE_TWO:
				//Log.i("Bluetooth", "Notification device two");
				gattTwoStreaming = true;
				if (gattOneStreaming && gattTwoStreaming && gattThreeStreaming && gattFourStreaming &&
						slidingWindowEnabled) {
					AccelerometerSample reading = (AccelerometerSample) msg.obj; 
					slidingWindowDeviceTwo.add(reading);
					//listSamplesDeviceTwo.add(reading);
				}
				break;

			case MSG_NEW_READING_DEVICE_THREE:
				//Log.i("Bluetooth", "Notification device three");
				gattThreeStreaming = true;
				if (gattOneStreaming && gattTwoStreaming && gattThreeStreaming && gattFourStreaming &&
						slidingWindowEnabled) {
					AccelerometerSample reading = (AccelerometerSample) msg.obj; 
					slidingWindowDeviceThree.add(reading);
					//listSamplesDeviceThree.add(reading);
				}
				break;

			case MSG_NEW_READING_DEVICE_FOUR:
				//Log.i("Bluetooth", "Notification device four");
				gattFourStreaming = true;
				if (gattOneStreaming && gattTwoStreaming && gattThreeStreaming && gattFourStreaming &&
						slidingWindowEnabled) {
					AccelerometerSample reading = (AccelerometerSample) msg.obj; 
					slidingWindowDeviceFour.add(reading);
					//listSamplesDeviceFour.add(reading);
				}
				break;

			default:
				Log.i("btHandler", "Silly message " + msg.toString());
				break;
			}

			if (slidingWindowEnabled && ((msg.what == MSG_NEW_READING_DEVICE_ONE) || 
					(msg.what == MSG_NEW_READING_DEVICE_TWO) || 
					(msg.what == MSG_NEW_READING_DEVICE_THREE) || 
					(msg.what == MSG_NEW_READING_DEVICE_FOUR)) ) {
				// check for if four device frame can be constructed
				boolean frameOneCompleted = slidingWindowDeviceOne.isFrameCompleted();
				boolean frameTwoCompleted = slidingWindowDeviceTwo.isFrameCompleted();
				boolean frameThreeCompleted = slidingWindowDeviceThree.isFrameCompleted();
				boolean frameFourCompleted = slidingWindowDeviceFour.isFrameCompleted();

				if (frameOneCompleted && frameTwoCompleted && frameThreeCompleted && frameFourCompleted) {
					AccelerometerFrame frameOne = slidingWindowDeviceOne.popFrame();
					//listFramesDeviceOne.add(frameOne);

					AccelerometerFrame frameTwo = slidingWindowDeviceTwo.popFrame();
					//listFramesDeviceTwo.add(frameTwo);

					AccelerometerFrame frameThree = slidingWindowDeviceThree.popFrame();
					//listFramesDeviceThree.add(frameThree);

					AccelerometerFrame frameFour = slidingWindowDeviceFour.popFrame();
					//listFramesDeviceFour.add(frameFour);

					long timeOne = frameOne.getStartTimestamp();
					long timeTwo = frameTwo.getStartTimestamp();
					long timeThree = frameThree.getStartTimestamp();
					long timeFour = frameFour.getStartTimestamp();

					if (!((timeOne == timeTwo) && (timeTwo == timeThree) && 
							(timeThree == timeFour) && (timeFour == timeOne))) {
						throw new RuntimeException("Misaligned frames");
					}

					AccelerometerFrameFourDevices frame4d = new AccelerometerFrameFourDevices(frameOne, frameTwo, frameThree, frameFour);
					Log.i("Accelerometry", "frame4d created (" + frameOne.getSize() + ", " +
							frameTwo.getSize() + ", " + frameThree.getSize() + ", " + frameFour.getSize() + ")" +
							"[numFrame = " + numFrame++ + "]");

					frame4d.computeFeatures();
					//Instance currentInstance = frame4d.toWekaInstance(trainingSet);

					//if (clusterDrivenModel == null) {
					//	listUnscaledFrames.add(frame4d);
					//}


					/*
					if (!frc.percentilesComputed()) {
						Log.i("FRC", "Adding frame to FRC database");
						frc.add(frame4d.getCopyWithoutSignalData());
					} else {
						Log.i("FRC", "NOT adding frame to FRC database");
					}
					 */

					frc.add(frame4d);

					Instance instanceUnscaled = FrameToInstanceConverter.convert(frame4d, trainingSet);

					//unlabelledData.add(FrameToInstanceConverter.convert(frame4d, unlabelledData));
					//Instance unscaledInstance = FrameToInstanceConverter.convert(frame4d, unlabelledData);
					//listUnlabelledInstances.add(unscaledInstance);
					//listUnscaledFrames.add(frame4d);
					//if (frc.percentilesComputed()) {
					AccelerometerFrameFourDevices frame4dScaled = frc.scaleFrame(frame4d);

					Instance scaledInstance = FrameToInstanceConverter.convert(frame4dScaled, unlabelledData);

					//unlabelledData.add(scaledInstance);
					//listUnlabelledInstances.add(scaledInstance);
					//Log.i("Unlabelled data", "Added unlabelled (scaled) frame to list");

					try {
						long t = frame4d.getStartTimestamp();

						Instance bigInstance = FrameToInstanceConverter.convert(frame4d, trainingSet);
						double[] bigDistr = model.distributionForInstance(bigInstance);
						double bigConfidence = -1;
						int confIndex = -1;
						for (int i = 0; i < bigDistr.length; i++) {
							if (bigDistr[i] > bigConfidence) {
								bigConfidence = bigDistr[i];
								confIndex = i;
							}
						}

						final double finalBigConfidence = bigConfidence;
						final int finalConfIndex = confIndex;
						runOnUiThread(new Runnable() {

							@Override
							public void run() {
								txvActivity.setText(ACTIVITY_LABELS[finalConfIndex]);
								txvConfidence.setText("Confidence: " + df.format(finalBigConfidence));
								txvTimestamp.setText(sdf.format(new Date()));
							}

						});

						popPredictedClassIndex = (int) popModel.classifyInstance(bigInstance);
						
						// 
						// - this is where the confidence of the 'activity discovery' model was calculated
						// - replace it with the 'confidence' of the population model
						
						//double smallConfidence = -1;
						//Instance smallInstance = DatasetAttributeFilter.getLowDimDataset(bigInstance, smallTrainingSet);
						//double[] smallDistr = smallModel.distributionForInstanceNonNormalized(smallInstance);
						//for (double conf : smallDistr) {
						//	if (conf > smallConfidence) {
						//		smallConfidence = conf;
						//	}
						//}

						//Log.i("Flushing", "t=" + t + "; smallConf=" + df.format(smallConfidence) + "; bigConf=" + bigConfidence);
						flushUnlabelledFrame(frame4d, t, 1, bigConfidence);
					} catch (Exception e) {
						throw new RuntimeException(e);
					}
					//flushUnlabelledInstances(true);
					//flushListOfSamples(true);
					//flushSamplesInFrame(frame4d);

					//double askProbNonNorm = 0;
					double askProbNorm = 0;
					boolean ask = false;

					try {
						//double[] labelDistr = model.distributionForInstance(currentInstance);
						List<Instance> segmentInstances;
						List<AccelerometerFrameFourDevices> segmentFrames;

						if (isInteractive) {
							sbd.add(frame4d, instanceUnscaled, frame4dScaled, scaledInstance);
							runOnUiThread(new Runnable() {
								@Override
								public void run() {
									Toast t = Toast.makeText(getApplicationContext(), 
											"Distance = " + df.format(sbd.getLastDistance()), Toast.LENGTH_SHORT);
									t.setGravity(Gravity.CENTER, 0, 0);
									t.show();
								}
							}); 

							boolean forcedSegment = false;
							//Log.i("SBD", "numFramesInBuffer(): " + sbd.numFramesInBuffer());
							if (sbd.numFramesInBuffer() >= MAX_SEGMENT_SIZE) {
								sbd.forceSegmentBoundaryNow();
								forcedSegment = true;
							}

							if (sbd.segmentBoundaryDetected()) {
								Log.i("Annotation", "segment boundary detected");
								segmentInstances = sbd.getWekaInstancesInSegment();
								segmentFrames = sbd.getFramesInSegment();
								Log.i("OMA", "Num frames in segment = " + segmentFrames.size());

								LabelInquiryDecident smallLid = new LabelInquiryDecident();
								LabelInquiryDecident bigLid = new LabelInquiryDecident();

								int n = segmentInstances.size();
								for (int i = 0; i < segmentInstances.size(); i++) {
									Instance instance = segmentInstances.get(i);
									//double[] probDistr = model.distributionForInstance(instance);

									//double[] probDistr = model.distributionForInstanceNonNormalized(instance);

									// 
									// replace these with population model 'confidence'
									//Instance smallInstance = DatasetAttributeFilter.getLowDimDataset(instance, smallTrainingSet);
									//double[] probDistrNonNorm = smallModel.distributionForInstanceNonNormalized(smallInstance);
									
									//smallLid.addProb(probDistrNonNorm, n);
									
									double[] probDistrNorm = model.distributionForInstance(instance);
									bigLid.addProb(probDistrNorm, n);
								}

								//askProbNonNorm = smallLid.askProb(smallGamma, smallMetric, smallBetaFunction);
								askProbNorm = bigLid.askProb(bigGamma, bigMetric, bigBetaFunction);

								Date now = new Date();
								long timestampNow = now.getTime();
								if (timestampNow > HEURISTIC_SWITCH_TIMESTAMP) {
									askProbNorm = Math.max(askProbNorm, MIN_ASK_PROB);
								} 

								final int finalConf = (int) (bigLid.averageConfidence() * 100);
								final String finalActivityLabel = ACTIVITY_LABELS[bigLid.getIndexOfMode()];
								final int finalNumFramesInSegment = segmentInstances.size();
								
								//if (!POP_MODEL_COVERAGE[popPredictedClassIndex]) {
								//	askProbNorm = 1;
								//}
								
								final double finalAskProbNorm = askProbNorm;

								runOnUiThread(new Runnable() {

									@Override
									public void run() {										
										txvTimestamp.setText(sdf.format(new Date()));
										txvActivity.setText(finalActivityLabel);									
										txvConfidence.setText("Confidence: " + finalConf + "%(" + 
												df.format(finalAskProbNorm) + ")");
										txvNumFrames.setText(finalNumFramesInSegment + " frames in segment (dist = " + 
												df.format(sbd.getDecisionDistance()) + ")");
										txvBudgetSize.setText("Budget spent: " + budgetSpent + " / " + BUDGET_SIZE);

										/*
										Toast t = Toast.makeText(getApplicationContext(), 
												String.valueOf(finalAskProbNonNorm) + " | " + String.valueOf(finalAskProbNorm), 
												Toast.LENGTH_SHORT);
										t.setGravity(Gravity.BOTTOM, 0, 0);
										t.show();
										*/
									}		

								});

								
								double randThresh;
								
								// 
								//Log.w("Annotation", "ask prob = " + askProbNonNorm + " (non-normalized)");
								//randThresh = rand.nextDouble();
								//Log.w("Annotation", "ask thresh = " + randThresh);
								//boolean smallAsk = randThresh < askProbNonNorm; 

								Log.w("Annotation", "ask prob = " + askProbNorm + " (normalized)");
								randThresh = rand.nextDouble();
								Log.w("Annotation", "ask thresh = " + randThresh);
								boolean bigAsk = randThresh < askProbNorm;

								//ask = randThresh < askProb;
								boolean popAsk = !popModelCoverage[popPredictedClassIndex]; 
								
								if (popAsk) {
									Log.w("Asking", "Because of Pop model (" + popPredictedClassIndex + ")");
								}
								
								/*
								if (!allClassesCovered) {
									boolean uncoveredFound = false;
									for (int i = 0; i < CLASS_COVERAGE.length; i++) {
										if (CLASS_COVERAGE[i] <= 0) {
											uncoveredFound = true;
											break;
										}
									}
									if (!uncoveredFound) {
										allClassesCovered = true;
									}
								}

								if (!allClassesCovered) {
									ask = smallAsk || bigAsk;
								} else {
									ask = bigAsk;
								}
								*/

								ask = bigAsk || popAsk;
								
								//ask = true;
								//ask = false;
								Log.w("Annotation", "ask = " + ask + "(" + popAsk + "|" + bigAsk + ")");
								++segmentCount;
								if (ask && (budgetSpent < BUDGET_SIZE)) {
									Log.i("Annotation", "Asking...");
									if (!physicalActivityDialogueActivated) {
										physicalActivityDialogueActivated = true;
										Log.i("Interactivity", "Invoking dialogue");

										runOnUiThread(new Runnable() {
											@Override
											public void run() {
												if (useAudibleTone) {
													// audible attention grabber
													AudioManager audioManager = (AudioManager) getSystemService(Context.AUDIO_SERVICE);
													final int volume = 100 * audioManager.getStreamVolume(AudioManager.STREAM_DTMF) / 15;

													ToneGenerator tg = new ToneGenerator(AudioManager.STREAM_ALARM, volume);
													tg.startTone(ToneGenerator.TONE_CDMA_HIGH_PBX_SLS, 1000);
												}
												Vibrator vibrator = (Vibrator) getApplicationContext()
														.getSystemService(Context.VIBRATOR_SERVICE);
												long[] vibratorPattern = {0, 
														500, 500,
														500, 500,
														500, 500};
												vibrator.vibrate(vibratorPattern, -1);

												Intent intent = new Intent(getApplicationContext(), LabelCollectionActivity.class);
												stopAccelerometerMonitoring(false);
												startActivityForResult(intent, REQUEST_CODE_USER_ANNOTATION);
											}
										});
									}
								} else {
									if (forcedSegment) {
										sbd.start();
									}
									Log.i("Annotation", "Not asking");
									/*
									runOnUiThread(new Runnable() {
										public void run() {
											Toast t = Toast.makeText(getApplicationContext(), "Not asking", Toast.LENGTH_SHORT);
											t.setGravity(Gravity.CENTER, 0, 0);
											t.show();
										}
									});
									Log.i("Toast", "Showed toast with message: " + "Not asking");
									 */
									
									Log.i("Not asking", "segmentInstances.size() = " + segmentInstances.size());

									Iterator<Instance> iteratorInstances = segmentInstances.iterator();
									Iterator<AccelerometerFrameFourDevices> iteratorFrames = segmentFrames.iterator();

									for (int i = 0; i < segmentInstances.size(); i++) {
										Instance instance = iteratorInstances.next();
										AccelerometerFrameFourDevices frame = iteratorFrames.next();

										instance.setValue(attActivity, -2);

										InstancePOJO pojo = new InstancePOJO(instance, segmentCount, 
												frame.getStartTimestamp(), frame.getEndTimestamp());
										flushLabelledPOJO(pojo); 
									}	
								}
							}
						}

					} catch (Exception e) {
						throw new RuntimeException(e);
					}
				}
			}
		}		
	};

	private final static int MSG_FLUSH_UNLABELLED_FRAME_4_D = 9001;
	private final static int MSG_FLUSH_LABELLED_INSTANCE_POJO= 9002;
	private final static int MSG_FLUSH_ONBOARD_SAMPLES = 9003;

	Handler dbFlusherHandler = new Handler() {
		@Override
		public void handleMessage(Message msg) {
			AccelerometerFrameFourDevices f4d;
			Instance instance;
			InstancePOJO pojo;

			switch (msg.what) {
			case MSG_FLUSH_UNLABELLED_FRAME_4_D:
				int thisCallID = ++callID;

				// flush instance
				f4d = (AccelerometerFrameFourDevices) msg.obj;
				instance = FrameToInstanceConverter.convert(f4d, unlabelledData);
				Bundle bundle = msg.getData();
				double smallConfidence = bundle.getDouble("smallConfidence");
				double bigConfidence = bundle.getDouble("bigConfidence");
				long timestamp = bundle.getLong("timestamp");
				instDs.saveUnlabelledInstance(instance, timestamp, smallConfidence, bigConfidence);
				//Log.i("DB Flusher", "Saved unlabelled instance");

				// flush readings to file
				AccelerometerFrame[] deviceFrames = new AccelerometerFrame[]{
						f4d.getFrameDeviceOne(), f4d.getFrameDeviceTwo(), 
						f4d.getFrameDeviceThree(), f4d.getFrameDeviceFour()
				};
				for (int devNum = 1; devNum <= 4; devNum++) {
					AccelerometerFrame frame = deviceFrames[devNum - 1];
					int numSamples = frame.getX().length;
					Log.i("DB", "Flushing " + numSamples + " samples for device " + devNum + 
							" [thisCallID = " + thisCallID + "]");
					double[] x = frame.getX();
					double[] y = frame.getY();
					double[] z = frame.getZ();
					long[] t = frame.getT();
					Log.i("Device " + devNum, "numSamples: " + x.length + "; " + 
							y.length + "; " + z.length);
					for (int i = 0; i < numSamples; i++) {
						AccelerometerSample sample = new AccelerometerSample(x[i], y[i], z[i], t[i]);
						instDs.appendReadingBytes(sample, devNum);
					}
					//Log.i("DB", "Done");
				}

				/*
				// flush readings
				AccelerometerFrame[] deviceFrames = new AccelerometerFrame[]{
						f4d.getFrameDeviceOne(), f4d.getFrameDeviceTwo(), 
						f4d.getFrameDeviceThree(), f4d.getFrameDeviceFour()
				};
				for (int devNum = 1; devNum <= 4; devNum++) {
					AccelerometerFrame frame = deviceFrames[devNum - 1];
					int numSamples = frame.getX().length;
					Log.i("DB", "Flushing " + numSamples + " samples for device " + devNum + 
							" [thisCallID = " + thisCallID + "]");
					double[] x = frame.getX();
					double[] y = frame.getY();
					double[] z = frame.getZ();
					long[] t = frame.getT();
					for (int i = 0; i < numSamples; i++) {
						AccelerometerSample sample = new AccelerometerSample(x[i], y[i], z[i], t[i]);
						instDs.saveReading(sample, devNum);
					}
					Log.i("DB", "Done");
				}
				 */
				break;
			case MSG_FLUSH_LABELLED_INSTANCE_POJO:
				pojo = (InstancePOJO) msg.obj;
				instDs.saveInstancePOJO(pojo);
				Log.w("DB Flusher", "Saved labelled instance" + pojo.getInstance().classValue());				
				break;
				/*
			case MSG_FLUSH_READINGS:
				@SuppressWarnings("unchecked")
				List<AccelerometerSample> readingsList = (List<AccelerometerSample>) msg.obj;
				int deviceNumber = msg.arg1;
				Log.i("DB", "Flushing " + readingsList.size() + " samples from device " + deviceNumber);
				for (AccelerometerSample reading : readingsList) {
					instDs.appendReadingBytes(reading, deviceNumber);
				}
				break;
				 */
			case MSG_FLUSH_ONBOARD_SAMPLES:
				@SuppressWarnings("unchecked")
				LinkedList<AccelerometerSample> list = (LinkedList<AccelerometerSample>) msg.obj;
				instDs.saveOnboardSamples(list);
				Log.i("DB Flusher", "Flushed " + list.size() + " onboard samples");
				break;
			default:
				Log.i("DB Handler", "Silly message " + msg.what);
				throw new RuntimeException("Silly message " + msg.what);
			}
		}
	};

	private final static UUID UUID_SERIAL_PORT_SERVICE = UUID.fromString("6e400001-b5a3-f393-e0a9-e50e24dcca9e");
	private final static UUID UUID_SERIAL_DATA_OUTPUT = UUID.fromString("6e400003-b5a3-f393-e0a9-e50e24dcca9e"); // -> 0x0001

	private final static UUID UUID_SENSOR_DATA_AND_COMMAND_SERVICE = UUID.fromString("00000000-0008-a8ba-e311-f48c90364d99");
	private final static UUID UUID_ENUMERATED_COMMAND_INPUT = UUID.fromString("00000001-0008-a8ba-e311-f48c90364d99"); // -> 0x0001
	private final static UUID UUID_SENSOR_DATA = UUID.fromString("00000002-0008-a8ba-e311-f48c90364d99"); // -> 0x0001

	private final static UUID UUID_SERVICE_SETTINGS_AND_STATUS_SERVICE = UUID.fromString("00000005-0008-a8ba-e311-f48c90364d99");
	private final static UUID UUID_SAMPLE_RATE = UUID.fromString("0000000a-0008-a8ba-e311-f48c90364d99");
	private final static UUID UUID_ACCELERATION_ON = UUID.fromString("0000000b-0008-a8ba-e311-f48c90364d99");
	private final static UUID UUID_ACCELERATION_RATE = UUID.fromString("0000000c-0008-a8ba-e311-f48c90364d99");
	private final static UUID UUID_ACCELERATION_RANGE = UUID.fromString("0000000d-0008-a8ba-e311-f48c90364d99");
	private final static UUID UUID_GYRO_OFF = UUID.fromString("0000000e-0008-a8ba-e311-f48c90364d99");
	private final static UUID UUID_MAGN_OFF = UUID.fromString("00000011-0008-a8ba-e311-f48c90364d99");
	private final static UUID UUID_LED = UUID.fromString("00000007-0008-a8ba-e311-f48c90364d99");

	private final static UUID UUID_CLIENT_CHARACTERISTIC_CONFIGURATION = UUID.fromString("00002902-0000-1000-8000-00805f9b34fb");
	//private final static UUID UUID_CHARACTERISTIC_USER_DESCRIPTION = UUID.fromString("00002901-0000-1000-8000-00805f9b34fb");

	@Override
	public void onSensorChanged(SensorEvent event) {
		if (event.sensor.getType() == Sensor.TYPE_ACCELEROMETER) {
			float accX = event.values[0];
			float accY = event.values[1];
			float accZ = event.values[2];
			long timestamp = event.timestamp;

			AccelerometerSample sample = new AccelerometerSample(accX, 
					accY, accZ, timestamp);
			listSamplesOnboard.add(sample);
			if (listSamplesOnboard.size() > 500) {
				synchronized(listSamplesOnboard) {
					flushOnboardSamples();
					listSamplesOnboard = new LinkedList<AccelerometerSample>();
				}
			}
		}
	}

	@Override
	protected void onCreate(Bundle savedInstanceState) {		
		Log.i("android state", "onCreate() called");
		super.onCreate(savedInstanceState);

		PowerManager powerManager = (PowerManager) getSystemService(Context.POWER_SERVICE);
		wakeLock = powerManager.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "MyScreenLock");

		setContentView(R.layout.activity_online_monitor);

		filesDir = getApplicationContext().getFilesDir().getPath().toString();
		
		//x = (TextView) findViewById(R.id.x);
		//y = (TextView) findViewById(R.id.y);
		//z = (TextView) findViewById(R.id.z);

		txvTimestamp = (TextView) findViewById(R.id.t);
		//meanX = (TextView) findViewById(R.id.meanX);
		//meanY = (TextView) findViewById(R.id.meanY);
		//meanZ = (TextView) findViewById(R.id.meanZ);
		//varX = (TextView) findViewById(R.id.varX);
		//varY = (TextView) findViewById(R.id.varY);
		//varZ = (TextView) findViewById(R.id.varZ);
		//corXY = (TextView) findViewById(R.id.corXY);
		//corYZ = (TextView) findViewById(R.id.corYZ);
		//corZX = (TextView) findViewById(R.id.corZX);

		// auxiliary features
		//sumX = (TextView) findViewById(R.id.sumX);
		//sumY = (TextView) findViewById(R.id.sumY);
		//sumZ = (TextView) findViewById(R.id.sumZ);
		//sumSqX = (TextView) findViewById(R.id.sumSqX);
		//sumSqY = (TextView) findViewById(R.id.sumSqY);
		//sumSqZ = (TextView) findViewById(R.id.sumSqZ);
		//sumXY = (TextView) findViewById(R.id.sumXY);
		//sumYZ = (TextView) findViewById(R.id.sumYZ);
		//sumZX = (TextView) findViewById(R.id.sumZX);

		txvNumFrames = (TextView) findViewById(R.id.numFrames);

		txvActivity = (TextView) findViewById(R.id.predictedActivity);
		txvConfidence = (TextView) findViewById(R.id.confidence);

		txvBudgetSize = (TextView) findViewById(R.id.budgetSpent);

		btnBell = (Button) findViewById(R.id.bell);
		btnBell.setOnClickListener(new View.OnClickListener() {

			@Override
			public void onClick(View v) {
				if (useAudibleTone) {
					btnBell.setText("Bell OFF");
				} else {
					btnBell.setText("Bell ON");

					AudioManager audioManager = (AudioManager) getSystemService(Context.AUDIO_SERVICE);
					final int volume = 100 * audioManager.getStreamVolume(AudioManager.STREAM_DTMF) / 15;
					Log.i("Tone Manager", "ringer volume = " + volume);

					ToneGenerator tg = new ToneGenerator(AudioManager.STREAM_ALARM, volume);					
					tg.startTone(ToneGenerator.TONE_CDMA_ABBR_ALERT, 150);
				}

				useAudibleTone = !useAudibleTone;
			}
		});

		pauseResume = (Button) findViewById(R.id.pauseResume);
		pauseResume.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				if (paused) {
					startAccelerometerMonitoring(false);
				} else {
					stopAccelerometerMonitoring(false);

					//flushListOfInstancePOJOs(true);
					//flushListOfSamples(true);
					//flushSamplesInFrame(frame4d);
					//flushUnlabelledInstances(true);
				}
			}			
		});

		/*
		delete = (Button) findViewById(R.id.clearDatabase);
		delete.setOnClickListener(new View.OnClickListener() {			
			@Override
			public void onClick(View v) {
				stopAccelerometerMonitoring(false);
				instDs.deleteAllInstances();
				instDs.deleteAllUnlabelledInstances();
				instDs.deleteAllReadings();
				instDs.vacuum();

				createTrainingSet();

				Toast t = Toast.makeText(OnlineMonitorActivity.this, 
						"Database cleared", Toast.LENGTH_SHORT);
				t.show();
			}
		});
		 */

		makeInteractive = (Button) findViewById(R.id.makeInteractive);
		makeInteractive.setOnClickListener(new View.OnClickListener() {

			@Override
			public void onClick(View v) {
				if (isInteractive) {
					makeInteractive.setBackgroundColor(
							getResources().getColor(R.color.yellow));
					makeInteractive.setText("Make Interactive \n(Currently Not Interactive)");
				} else {
					//Log.i("Percentiles", "Invoking PercentileCalculatorTask...");
					//PercentileCalculatorTask task = new PercentileCalculatorTask();
					//task.execute(frc);
					//flushListOfInstancePOJOs(true);
					//flushListOfSamples(true);
					//flushSamplesInFrame(frame4d);

					makeInteractive.setEnabled(true);
					//startAccelerometerMonitoring(false);
					makeInteractive.setBackgroundColor(
							getResources().getColor(R.color.green));
					makeInteractive.setText("Make Non Interactive \n(Currently Interactive)");
				}
				isInteractive = !isInteractive;
				Log.i("Annotation", "Is interactive: " + isInteractive);
			}
		});

		createSchema();
		createSchemaUnlabelled();

		openDBConnection();
		createTrainingSet();

		// load frc from file, if exists
		try {
			FileInputStream fis = openFileInput(FILE_NAME_FRC);
			ObjectInputStream ois = new ObjectInputStream(fis);
			frc = (FeatureRangeCalculatorLowMemory) ois.readObject();
			//frc = (FeatureRangeCalculator) ois.readObject();
		} catch (Exception e) {
			frc = new FeatureRangeCalculatorLowMemory(FRC_TAIL_LENGTH);
			//frc = new FeatureRangeCalculator(MIN_PERCENTIlE, MAX_PERCENTILE);
		}

		physicalActivityDialogueActivated = false;

		if (paused) {
			stopAccelerometerMonitoring(false);
		} else {
			startAccelerometerMonitoring(false);
		}

		// Bluetooth init
		final BluetoothManager bluetoothManager = (BluetoothManager) getSystemService(Context.BLUETOOTH_SERVICE);
		bluetoothAdapter = bluetoothManager.getAdapter();
		if ((bluetoothAdapter == null) || !bluetoothAdapter.isEnabled()) {
			Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
			startActivityForResult(enableBtIntent, REQUEST_CODE_ENABLE_BLUETOOTH);
			Log.i("Bluetooth", "Bluetooth is OFF. Enabling...");
		}

		// BLE
		scanLeDevice(true);
		
		// transition time between heuristics
		GregorianCalendar cal = new GregorianCalendar();
		
		cal.set(Calendar.AM_PM, Calendar.AM);
		cal.set(Calendar.HOUR, SWITCH_OVER_HOUR);
		cal.set(Calendar.MINUTE, 0);
		cal.set(Calendar.SECOND, 0);
		
		Date future = cal.getTime();
		HEURISTIC_SWITCH_TIMESTAMP = future.getTime();
	}

	void scanLeDevice(boolean enable) {
		final long SCAN_PERIOD = 10000; // 10 seconds
		if (enable) {
			Log.i("Bluetooth", "Scanning...");
			//scanningInProgress = true;
			bluetoothAdapter.startLeScan(leScanCallback);

			bluetoothScanHandler = new Handler();
			bluetoothScanHandler.postDelayed(new Runnable() {

				@Override
				public void run() {
					//scanningInProgress = false;

					bluetoothAdapter.stopLeScan(leScanCallback);
					Log.i("Bluetooth", "Cancelling BT discovery");
					bluetoothAdapter.cancelDiscovery();
					Log.i("Bluetooth", "Stopped LE scanning");

					/*
					Log.i("Bluetooth", "Naping 5 seconds");
					try {
						Thread.sleep(5000);
					} catch (InterruptedException e) {
						e.printStackTrace();
						throw new RuntimeException(e);
					}
					 */

					// connect to device one
					Log.i("Bluetooth", "Now starting to connect");
					if (btDeviceOne != null) {
						btHandler.sendMessage(Message.obtain(null, MSG_CONNECT_GATT, btDeviceOne.getAddress()));
					} else {
						Log.w("Bluetooth", "Did not discover " + btAddrOne);		
						throw new RuntimeException("Did not discover " + btAddrOne);
					}

					// connect to device two
					if (btDeviceTwo != null) {
						btHandler.sendMessage(Message.obtain(null, MSG_CONNECT_GATT, btDeviceTwo.getAddress()));
					} else {
						Log.w("Bluetooth", "Did not discover " + btAddrTwo);
						throw new RuntimeException("Did not discover " + btAddrTwo);
					}

					// connect to device three
					if (btDeviceThree != null) {
						btHandler.sendMessage(Message.obtain(null, MSG_CONNECT_GATT, btDeviceThree.getAddress()));
					} else {
						Log.w("Bluetooth", "Did not discover " + btDeviceThree);
						throw new RuntimeException("Did not discover " + btAddrThree);
					}

					// connect to device four
					if (btDeviceFour != null) {
						btHandler.sendMessage(Message.obtain(null, MSG_CONNECT_GATT, btDeviceFour.getAddress()));
					} else {
						Log.w("Bluetooth", "Did not discover " + btDeviceFour);
						throw new RuntimeException("Did not discover " + btAddrFour);
					}
				}

			}, SCAN_PERIOD);
		} else {
			//scanningInProgress = false;
			bluetoothAdapter.stopLeScan(leScanCallback);
		}
	}

	private void createSchema() {
		schema = new FastVector();

		attMeanXDeviceOne = new Attribute("meanXdeviceOne");
		schema.addElement(attMeanXDeviceOne);
		attMeanYDeviceOne = new Attribute("meanYdeviceOne");
		schema.addElement(attMeanYDeviceOne);
		attMeanZDeviceOne = new Attribute("meanZdeviceOne");
		schema.addElement(attMeanZDeviceOne);
		attVarXDeviceOne = new Attribute("varXdeviceOne");
		schema.addElement(attVarXDeviceOne);
		attVarYDeviceOne = new Attribute("varYdeviceOne");
		schema.addElement(attVarYDeviceOne);
		attVarZDeviceOne = new Attribute("varZdeviceOne");
		schema.addElement(attVarZDeviceOne);
		attCorXYDeviceOne = new Attribute("corXYdeviceOne");
		schema.addElement(attCorXYDeviceOne);
		attCorYZDeviceOne = new Attribute("corYZdeviceOne");
		schema.addElement(attCorYZDeviceOne);
		attCorZXDeviceOne = new Attribute("corZXdeviceOne");
		schema.addElement(attCorZXDeviceOne);

		attMeanXDeviceTwo = new Attribute("meanXdeviceTwo");
		schema.addElement(attMeanXDeviceTwo);
		attMeanYDeviceTwo = new Attribute("meanYdeviceTwo");
		schema.addElement(attMeanYDeviceTwo);
		attMeanZDeviceTwo = new Attribute("meanZdeviceTwo");
		schema.addElement(attMeanZDeviceTwo);
		attVarXDeviceTwo = new Attribute("varXdeviceTwo");
		schema.addElement(attVarXDeviceTwo);
		attVarYDeviceTwo = new Attribute("varYdeviceTwo");
		schema.addElement(attVarYDeviceTwo);
		attVarZDeviceTwo = new Attribute("varZdeviceTwo");
		schema.addElement(attVarZDeviceTwo);
		attCorXYDeviceTwo = new Attribute("corXYdeviceTwo");
		schema.addElement(attCorXYDeviceTwo);
		attCorYZDeviceTwo = new Attribute("corYZdeviceTwo");
		schema.addElement(attCorYZDeviceTwo);
		attCorZXDeviceTwo = new Attribute("corZXdeviceTwo");
		schema.addElement(attCorZXDeviceTwo);

		attMeanXDeviceThree = new Attribute("meanXdeviceThree");
		schema.addElement(attMeanXDeviceThree);
		attMeanYDeviceThree = new Attribute("meanYdeviceThree");
		schema.addElement(attMeanYDeviceThree);
		attMeanZDeviceThree = new Attribute("meanZdeviceThree");
		schema.addElement(attMeanZDeviceThree);
		attVarXDeviceThree = new Attribute("varXdeviceThree");
		schema.addElement(attVarXDeviceThree);
		attVarYDeviceThree = new Attribute("varYdeviceThree");
		schema.addElement(attVarYDeviceThree);
		attVarZDeviceThree = new Attribute("varZdeviceThree");
		schema.addElement(attVarZDeviceThree);
		attCorXYDeviceThree = new Attribute("corXYdeviceThree");
		schema.addElement(attCorXYDeviceThree);
		attCorYZDeviceThree = new Attribute("corYZdeviceThree");
		schema.addElement(attCorYZDeviceThree);
		attCorZXDeviceThree = new Attribute("corZXdeviceThree");
		schema.addElement(attCorZXDeviceThree);

		attMeanXDeviceFour = new Attribute("meanXdeviceFour");
		schema.addElement(attMeanXDeviceFour);
		attMeanYDeviceFour = new Attribute("meanYdeviceFour");
		schema.addElement(attMeanYDeviceFour);
		attMeanZDeviceFour = new Attribute("meanZdeviceFour");
		schema.addElement(attMeanZDeviceFour);
		attVarXDeviceFour = new Attribute("varXdeviceFour");
		schema.addElement(attVarXDeviceFour);
		attVarYDeviceFour = new Attribute("varYdeviceFour");
		schema.addElement(attVarYDeviceFour);
		attVarZDeviceFour = new Attribute("varZdeviceFour");
		schema.addElement(attVarZDeviceFour);
		attCorXYDeviceFour = new Attribute("corXYdeviceFour");
		schema.addElement(attCorXYDeviceFour);
		attCorYZDeviceFour = new Attribute("corYZdeviceFour");
		schema.addElement(attCorYZDeviceFour);
		attCorZXDeviceFour = new Attribute("corZXdeviceFour");
		schema.addElement(attCorZXDeviceFour);


		FastVector activities = new FastVector(ACTIVITY_LABELS.length);
		for (int i = 0; i < ACTIVITY_LABELS.length; i++) {
			activities.addElement(ACTIVITY_LABELS[i]);
		}

		attActivity = new Attribute("activity", activities);
		schema.addElement(attActivity);
	}

	private void createSchemaUnlabelled() {
		unlabelledSchema = new FastVector();

		attMeanXDeviceOne = new Attribute("meanXdeviceOne");
		unlabelledSchema.addElement(attMeanXDeviceOne);
		attMeanYDeviceOne = new Attribute("meanYdeviceOne");
		unlabelledSchema.addElement(attMeanYDeviceOne);
		attMeanZDeviceOne = new Attribute("meanZdeviceOne");
		unlabelledSchema.addElement(attMeanZDeviceOne);
		attVarXDeviceOne = new Attribute("varXdeviceOne");
		unlabelledSchema.addElement(attVarXDeviceOne);
		attVarYDeviceOne = new Attribute("varYdeviceOne");
		unlabelledSchema.addElement(attVarYDeviceOne);
		attVarZDeviceOne = new Attribute("varZdeviceOne");
		unlabelledSchema.addElement(attVarZDeviceOne);
		attCorXYDeviceOne = new Attribute("corXYdeviceOne");
		unlabelledSchema.addElement(attCorXYDeviceOne);
		attCorYZDeviceOne = new Attribute("corYZdeviceOne");
		unlabelledSchema.addElement(attCorYZDeviceOne);
		attCorZXDeviceOne = new Attribute("corZXdeviceOne");
		unlabelledSchema.addElement(attCorZXDeviceOne);

		attMeanXDeviceTwo = new Attribute("meanXdeviceTwo");
		unlabelledSchema.addElement(attMeanXDeviceTwo);
		attMeanYDeviceTwo = new Attribute("meanYdeviceTwo");
		unlabelledSchema.addElement(attMeanYDeviceTwo);
		attMeanZDeviceTwo = new Attribute("meanZdeviceTwo");
		unlabelledSchema.addElement(attMeanZDeviceTwo);
		attVarXDeviceTwo = new Attribute("varXdeviceTwo");
		unlabelledSchema.addElement(attVarXDeviceTwo);
		attVarYDeviceTwo = new Attribute("varYdeviceTwo");
		unlabelledSchema.addElement(attVarYDeviceTwo);
		attVarZDeviceTwo = new Attribute("varZdeviceTwo");
		unlabelledSchema.addElement(attVarZDeviceTwo);
		attCorXYDeviceTwo = new Attribute("corXYdeviceTwo");
		unlabelledSchema.addElement(attCorXYDeviceTwo);
		attCorYZDeviceTwo = new Attribute("corYZdeviceTwo");
		unlabelledSchema.addElement(attCorYZDeviceTwo);
		attCorZXDeviceTwo = new Attribute("corZXdeviceTwo");
		unlabelledSchema.addElement(attCorZXDeviceTwo);

		attMeanXDeviceThree = new Attribute("meanXdeviceThree");
		unlabelledSchema.addElement(attMeanXDeviceThree);
		attMeanYDeviceThree = new Attribute("meanYdeviceThree");
		unlabelledSchema.addElement(attMeanYDeviceThree);
		attMeanZDeviceThree = new Attribute("meanZdeviceThree");
		unlabelledSchema.addElement(attMeanZDeviceThree);
		attVarXDeviceThree = new Attribute("varXdeviceThree");
		unlabelledSchema.addElement(attVarXDeviceThree);
		attVarYDeviceThree = new Attribute("varYdeviceThree");
		unlabelledSchema.addElement(attVarYDeviceThree);
		attVarZDeviceThree = new Attribute("varZdeviceThree");
		unlabelledSchema.addElement(attVarZDeviceThree);
		attCorXYDeviceThree = new Attribute("corXYdeviceThree");
		unlabelledSchema.addElement(attCorXYDeviceThree);
		attCorYZDeviceThree = new Attribute("corYZdeviceThree");
		unlabelledSchema.addElement(attCorYZDeviceThree);
		attCorZXDeviceThree = new Attribute("corZXdeviceThree");
		unlabelledSchema.addElement(attCorZXDeviceThree);

		attMeanXDeviceFour = new Attribute("meanXdeviceFour");
		unlabelledSchema.addElement(attMeanXDeviceFour);
		attMeanYDeviceFour = new Attribute("meanYdeviceFour");
		unlabelledSchema.addElement(attMeanYDeviceFour);
		attMeanZDeviceFour = new Attribute("meanZdeviceFour");
		unlabelledSchema.addElement(attMeanZDeviceFour);
		attVarXDeviceFour = new Attribute("varXdeviceFour");
		unlabelledSchema.addElement(attVarXDeviceFour);
		attVarYDeviceFour = new Attribute("varYdeviceFour");
		unlabelledSchema.addElement(attVarYDeviceFour);
		attVarZDeviceFour = new Attribute("varZdeviceFour");
		unlabelledSchema.addElement(attVarZDeviceFour);
		attCorXYDeviceFour = new Attribute("corXYdeviceFour");
		unlabelledSchema.addElement(attCorXYDeviceFour);
		attCorYZDeviceFour = new Attribute("corYZdeviceFour");
		unlabelledSchema.addElement(attCorYZDeviceFour);
		attCorZXDeviceFour = new Attribute("corZXdeviceFour");
		unlabelledSchema.addElement(attCorZXDeviceFour);
	}

	private void openDBConnection() {
		instDs = new InstancesDataSource(this, schema);
		instDs.setFilesDir(getApplicationContext().getFilesDir().getPath().toString());
		instDs.open();
	}

	private void createTrainingSet() {
		//instDs.open();
		trainingSet = instDs.getAllInstances(true);
		unlabelledData = instDs.getUnlabelledInstances();

		trainingSet.setClass(attActivity);
		Log.i("Training Set", "Size = " + trainingSet.numInstances() + " instances.");
		
		//smallTrainingSet = DatasetAttributeFilter.getLowDimDataset(trainingSet, ACTIVITY_LABELS);
		//Log.i("Small Training Set", "Size = " + smallTrainingSet.numInstances() + " instances.");

		try {
			//model = new NaiveBayesUpdateable();
			///model = new MyNaiveBayes();
			model = new OnlineBagNaiveBayes(BAG_SIZE);
			model.buildClassifier(trainingSet);
			Log.w("Model", "Built model from " + trainingSet.numInstances() + " frames");
			
			File popTrainingSetFile = new File(filesDir + "/" + popTrainingFileName);
			ArffLoader arffLoader = new ArffLoader();
			arffLoader.setFile(popTrainingSetFile);
			Instances popTrainingSet = arffLoader.getDataSet();
						
			popTrainingSet.setClassIndex(popTrainingSet.numAttributes() - 1);
			Log.w("Model", popTrainingSet.numInstances() + " pop training instances loaded");
			
			popModel = new IB1();
			popModel.buildClassifier(popTrainingSet);
			Log.w("Model", "Built population model");
			
			// TODO load coverage
			try {
				FileInputStream fis = getApplicationContext().openFileInput(FILE_NAME_COVERAGE);
				ObjectInputStream ois = new ObjectInputStream(fis);
				popModelCoverage = new boolean[ACTIVITY_LABELS.length];
				for (int i = 0; i < popModelCoverage.length; i++) {
					popModelCoverage[i] = ois.readBoolean();
				}
				Log.w("Coverage", "Loaded pop coverage from file");
					
			} catch (Exception e) {
				// file does not exist
				Log.wtf("Coverage", "Coverage not on file");
			}
			//smallModel = new MyNaiveBayes();
			//smallModel.buildClassifier(smallTrainingSet);
			//Log.w("Model", "Built small model");

			//PercentileCalculatorTask pcTask = new PercentileCalculatorTask();
			//pcTask.execute(frc);

			
			budgetSpent = instDs.getNumAnnotatedSegments();
			Log.i("Budget", "Loaded from database. budgetSpent = " + budgetSpent);
			
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	//	private void persistStandaloneInstance(Instance instance, long firstTimestamp, long lastTimestamp) {
	//		instance.setValue(instance.numAttributes()-1, 0.0);
	//		StandaloneInstancePersisterTask task = new StandaloneInstancePersisterTask();
	//		task.execute(instance, firstTimestamp, lastTimestamp);
	//	}

	@Override
	protected void onResume() {
		Log.i("android state", "onResume() called");
		if (!wakeLock.isHeld()) {
			Log.i("Screen Locking", "Lock not held. OMA acquiring lock");
			wakeLock.acquire();
		} else {
			Log.i("Screen locking", "Lock is already held. No need to acquire");
		}
		// db init
		//instDs = new InstancesDataSource(this, schema);
		//instDs.open();

		super.onResume();
	};

	@Override
	public void onStart() {
		super.onStart();
		Log.i("android state", "onStart() called");
		if (!paused) {
			//startAccelerometerMonitoring(false);
		} else {
			//stopAccelerometerMonitoring(false);
		}
	}

	@Override
	protected void onPause() {
		Log.i("android state", "onPause() called");
		super.onPause();
	}

	@Override
	protected void onStop() {
		Log.i("android state", "onStop() called");
		super.onStop();

	}

	@Override
	protected void onDestroy() {
		//Log.i("Screen Locking", "OMA releasing lock in OMA.onDestroy()");
		//wakeLock.release();
		Log.i("Screen Locking", "Refusing to release wakeLock");
		//stopAccelerometerMonitoring(false);
		//instDs.close();
		super.onDestroy();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.activity_online_monitor, menu);
		return true;
	}

	/*
	@Override
	protected void onSaveInstanceState(Bundle outState) {
		Log.i("android state", "onSaveInstanceState() called");
		super.onSaveInstanceState(outState);
		outState.putBoolean("paused", paused);
		outState.putInt("segmentCount", segmentCount);
		outState.putSerializable("slidingWindow", slidingWindow);
		//outState.putSerializable("frame", frame);
		outState.putSerializable("model", model);
		outState.putSerializable("numUpdates", numUpdates);

		outState.putSerializable("numClusters", numClusters);
		outState.putSerializable("clustersCoverage", clustersCoverage);
		outState.putSerializable("clusterIndexVote", clusterIndexVote);
		outState.putSerializable("clusterDrivenModel", clusterDrivenModel);

		outState.putSerializable("schema", schema);

		outState.putSerializable("attMeanXDeviceOne", attMeanXDeviceOne);
		outState.putSerializable("attMeanYDeviceOne", attMeanYDeviceOne);
		outState.putSerializable("attMeanZDeviceOne", attMeanZDeviceOne);
		outState.putSerializable("attCorXYDeviceOne", attCorXYDeviceOne);
		outState.putSerializable("attCorYZDeviceOne", attCorYZDeviceOne);
		outState.putSerializable("attCorZXDeviceOne", attCorZXDeviceOne);
		outState.putSerializable("attVarXDeviceOne", attVarXDeviceOne);
		outState.putSerializable("attVarYDeviceOne", attVarYDeviceOne);
		outState.putSerializable("attVarZDeviceOne", attVarZDeviceOne);

		outState.putSerializable("attMeanXDeviceTwo", attMeanXDeviceTwo);
		outState.putSerializable("attMeanYDeviceTwo", attMeanYDeviceTwo);
		outState.putSerializable("attMeanZDeviceTwo", attMeanZDeviceTwo);
		outState.putSerializable("attCorXYDeviceTwo", attCorXYDeviceTwo);
		outState.putSerializable("attCorYZDeviceTwo", attCorYZDeviceTwo);
		outState.putSerializable("attCorZXDeviceTwo", attCorZXDeviceTwo);
		outState.putSerializable("attVarXDeviceTwo", attVarXDeviceTwo);
		outState.putSerializable("attVarYDeviceTwo", attVarYDeviceTwo);
		outState.putSerializable("attVarZDeviceTwo", attVarZDeviceTwo);

		outState.putSerializable("attMeanXDeviceThree", attMeanXDeviceThree);
		outState.putSerializable("attMeanYDeviceThree", attMeanYDeviceThree);
		outState.putSerializable("attMeanZDeviceThree", attMeanZDeviceThree);
		outState.putSerializable("attCorXYDeviceThree", attCorXYDeviceThree);
		outState.putSerializable("attCorYZDeviceThree", attCorYZDeviceThree);
		outState.putSerializable("attCorZXDeviceThree", attCorZXDeviceThree);
		outState.putSerializable("attVarXDeviceThree", attVarXDeviceThree);
		outState.putSerializable("attVarYDeviceThree", attVarYDeviceThree);
		outState.putSerializable("attVarZDeviceThree", attVarZDeviceThree);

		outState.putSerializable("attMeanXDeviceFour", attMeanXDeviceFour);
		outState.putSerializable("attMeanYDeviceFour", attMeanYDeviceFour);
		outState.putSerializable("attMeanZDeviceFour", attMeanZDeviceFour);
		outState.putSerializable("attCorXYDeviceFour", attCorXYDeviceFour);
		outState.putSerializable("attCorYZDeviceFour", attCorYZDeviceFour);
		outState.putSerializable("attCorZXDeviceFour", attCorZXDeviceFour);
		outState.putSerializable("attVarXDeviceFour", attVarXDeviceFour);
		outState.putSerializable("attVarYDeviceFour", attVarYDeviceFour);
		outState.putSerializable("attVarZDeviceFour", attVarZDeviceFour);

		outState.putSerializable("attActivity", attActivity);

		outState.putSerializable("trainingSet", trainingSet);
		outState.putSerializable("unlabelledData", unlabelledData);

		outState.putSerializable("useAudibleTone", useAudibleTone);
		outState.putSerializable("isInteractive", isInteractive);

		outState.putSerializable("frc", frc);
		outState.putSerializable("sbd", sbd);

		outState.putSerializable("listSamplesDeviceOne", listSamplesDeviceOne);
		outState.putSerializable("listSamplesDeviceTwo", listSamplesDeviceTwo);
		outState.putSerializable("listSamplesDeviceThree", listSamplesDeviceThree);
		outState.putSerializable("listSamplesDeviceFour", listSamplesDeviceFour);

		outState.putSerializable("listInstancePOJOs", listInstancePOJOs);
		outState.putSerializable("listUnlabelledInstances", listUnlabelledInstances);

		outState.putInt("numInstancesConfidentlyClassified", 
				numInstancesConfidentlyClassified);	
	}
	 */
	/*
	@SuppressWarnings("unchecked")
	@Override
	protected void onRestoreInstanceState(Bundle inState) {
		Log.i("android state", "onRestoreInstanceState() called");
		paused = inState.getBoolean("paused");

		segmentCount = (Integer) inState.getSerializable("segmentCount");

		slidingWindow = (SlidingWindow) inState.getSerializable("slidingWindow");
		//frame = (AccelerometerFrame) inState.getSerializable("frame");
		model = (NaiveBayesUpdateable) inState.getSerializable("model");
		numUpdates = (Integer) inState.getSerializable("numUpdates");

		numClusters = (Integer)inState.getSerializable("numClusters");
		clustersCoverage = (int[]) inState.getSerializable("clustersCoverage");
		clusterIndexVote = (Integer) inState.getSerializable("clusterIndexVote");
		clusterDrivenModel = (NaiveBayes) inState.getSerializable("clusterDrivenModel");

		schema = (FastVector) inState.getSerializable("schema");

		attMeanXDeviceOne = (Attribute) inState.getSerializable("attMeanXDeviceOne");
		attMeanYDeviceOne = (Attribute) inState.getSerializable("attMeanYDeviceOne");
		attMeanZDeviceOne = (Attribute) inState.getSerializable("attMeanZDeviceOne");
		attCorXYDeviceOne = (Attribute) inState.getSerializable("attCorXYDeviceOne");
		attCorYZDeviceOne = (Attribute) inState.getSerializable("attCorYZDeviceOne");
		attCorZXDeviceOne = (Attribute) inState.getSerializable("attCorZXDeviceOne");
		attVarXDeviceOne = (Attribute) inState.getSerializable("attVarXDeviceOne");
		attVarYDeviceOne = (Attribute) inState.getSerializable("attVarYDeviceOne");
		attVarZDeviceOne = (Attribute) inState.getSerializable("attVarZDeviceOne");

		attMeanXDeviceOne = (Attribute) inState.getSerializable("attMeanXDeviceOne");
		attMeanYDeviceOne = (Attribute) inState.getSerializable("attMeanYDeviceOne");
		attMeanZDeviceOne = (Attribute) inState.getSerializable("attMeanZDeviceOne");
		attCorXYDeviceOne = (Attribute) inState.getSerializable("attCorXYDeviceOne");
		attCorYZDeviceOne = (Attribute) inState.getSerializable("attCorYZDeviceOne");
		attCorZXDeviceOne = (Attribute) inState.getSerializable("attCorZXDeviceOne");
		attVarXDeviceOne = (Attribute) inState.getSerializable("attVarXDeviceOne");
		attVarYDeviceOne = (Attribute) inState.getSerializable("attVarYDeviceOne");
		attVarZDeviceOne = (Attribute) inState.getSerializable("attVarZDeviceOne");

		attMeanXDeviceOne = (Attribute) inState.getSerializable("attMeanXDeviceOne");
		attMeanYDeviceOne = (Attribute) inState.getSerializable("attMeanYDeviceOne");
		attMeanZDeviceOne = (Attribute) inState.getSerializable("attMeanZDeviceOne");
		attCorXYDeviceOne = (Attribute) inState.getSerializable("attCorXYDeviceOne");
		attCorYZDeviceOne = (Attribute) inState.getSerializable("attCorYZDeviceOne");
		attCorZXDeviceOne = (Attribute) inState.getSerializable("attCorZXDeviceOne");
		attVarXDeviceOne = (Attribute) inState.getSerializable("attVarXDeviceOne");
		attVarYDeviceOne = (Attribute) inState.getSerializable("attVarYDeviceOne");
		attVarZDeviceOne = (Attribute) inState.getSerializable("attVarZDeviceOne");

		attMeanXDeviceOne = (Attribute) inState.getSerializable("attMeanXDeviceOne");
		attMeanYDeviceOne = (Attribute) inState.getSerializable("attMeanYDeviceOne");
		attMeanZDeviceOne = (Attribute) inState.getSerializable("attMeanZDeviceOne");
		attCorXYDeviceOne = (Attribute) inState.getSerializable("attCorXYDeviceOne");
		attCorYZDeviceOne = (Attribute) inState.getSerializable("attCorYZDeviceOne");
		attCorZXDeviceOne = (Attribute) inState.getSerializable("attCorZXDeviceOne");
		attVarXDeviceOne = (Attribute) inState.getSerializable("attVarXDeviceOne");
		attVarYDeviceOne = (Attribute) inState.getSerializable("attVarYDeviceOne");
		attVarZDeviceOne = (Attribute) inState.getSerializable("attVarZDeviceOne");

		attActivity = (Attribute) inState.getSerializable("attActivity");

		trainingSet = (Instances) inState.getSerializable("trainingSet");
		unlabelledData = (Instances) inState.getSerializable("unlabelledData");

		useAudibleTone = inState.getBoolean("useAudibleTone");
		isInteractive = inState.getBoolean("isInteractive");

		frc = (FeatureRangeCalculator) inState.getSerializable("frc");
		sbd = (SegmentBoundaryDetector) inState.getSerializable("sbd");

		listSamplesDeviceOne = (ArrayList<AccelerometerSample>) inState.getSerializable("listSamplesDeviceOne");
		listSamplesDeviceTwo = (ArrayList<AccelerometerSample>) inState.getSerializable("listSamplesDeviceTwo");
		listSamplesDeviceThree = (ArrayList<AccelerometerSample>) inState.getSerializable("listSamplesDeviceThree");
		listSamplesDeviceFour = (ArrayList<AccelerometerSample>) inState.getSerializable("listSamplesDeviceFour");

		listInstancePOJOs = (ArrayList<InstancePOJO>) inState.getSerializable("listInstancePOJOs");
		listUnlabelledInstances = (ArrayList<Instance>) inState.getSerializable("listUnlabelledInstances");

		numInstancesConfidentlyClassified = 
				inState.getInt("numInstancesConfidentlyClassified");

		super.onRestoreInstanceState(inState);
	}
	 */

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		super.onActivityResult(requestCode, resultCode, data);
		Log.wtf("onActivityResult()", "resultCode = " + String.valueOf(resultCode));
		Log.wtf("onActivityResult()", "requestCode = " + REQUEST_CODE_USER_ANNOTATION);
		try {

			if (requestCode == REQUEST_CODE_USER_ANNOTATION) {

				List<Instance> segmentInstances = sbd.getWekaInstancesInSegment();
				List<AccelerometerFrameFourDevices> segmentFrames = sbd.getFramesInSegment();
				if ((segmentInstances == null) || (segmentFrames == null)) {
					return;
				}
				Iterator<Instance> iteratorInstances = segmentInstances.iterator();
				Iterator<AccelerometerFrameFourDevices> iteratorFrames = segmentFrames.iterator();

				Log.w("onActivityResult", "Class label: " + resultCode);

				Log.i("Annotation", "budgetSpent before = " + budgetSpent);
				if (resultCode >= 0) {
					// update NB_cluster voting coverage
					//CLASS_COVERAGE[resultCode] += segmentInstances.size();
					popModelCoverage[popPredictedClassIndex] = true;
					
					// persist pop coverage to secondary storage
					FileOutputStream fos;
					ObjectOutputStream oos;

					// persist model to file
					fos = getApplicationContext().openFileOutput(FILE_NAME_COVERAGE, Context.MODE_PRIVATE);
					oos = new ObjectOutputStream(fos);
					//oos.write(popModelCoverage);
					// TODO
					// write values
					
					for (int i = 0; i < popModelCoverage.length; i++) {
						oos.writeBoolean(popModelCoverage[i]);
					}					
					oos.close();
					Log.i("Persistence", "Persisted popModelCoverage in " + FILE_NAME_COVERAGE);
					
					budgetSpent++;
					Log.i("Annotation", "Useful label provided: " + resultCode);
				} else {
					Log.i("Annotation", "Label not valid: " + resultCode);
				}
				Log.i("Annotation", "budgetSpent after = " + budgetSpent);

				StringBuilder sb = new StringBuilder();
				for (int k = 0; k < popModelCoverage.length; k++) {
					int value = popModelCoverage[k] ? 1 : 0;
					if (popPredictedClassIndex == k) {
						sb.append("*");
						sb.append(value);
						sb.append("* ");
					} else {
						sb.append(value);
						sb.append(" ");
					}
				}
				Log.i("Pop Model", "Pop Class Coverage updated: " + sb.toString());

				for (int i = 0; i < segmentInstances.size(); i++) {
					Instance instance = iteratorInstances.next();
					AccelerometerFrameFourDevices frame = iteratorFrames.next();

					instance.setValue(attActivity, resultCode);

					if (resultCode >= 0) {
						//trainingSet.add(instance);
						model.updateClassifier(instance);

						//smallModel.updateClassifier(DatasetAttributeFilter.getLowDimDataset(instance, smallTrainingSet));
						
						++numUpdates;
						Log.i("Model", "NB updated " + numUpdates + " times so far");
						//Log.i("Model", "Training set size = " + trainingSet.numInstances());
						//segmentNumber = segmentCount;	
					} 
					/*else {
						segmentNumber = -1;
					}*/
					InstancePOJO pojo = new InstancePOJO(instance, segmentCount, 
							frame.getStartTimestamp(), frame.getEndTimestamp());
					flushLabelledPOJO(pojo); 
				}	
				// persist model
				FileOutputStream fos;
				ObjectOutputStream oos;

				// persist model to file
				fos = getApplicationContext().openFileOutput(FILE_NAME_NB_MODEL, Context.MODE_PRIVATE);
				oos = new ObjectOutputStream(fos);
				oos.writeObject(model);
				oos.close();
				Log.i("Persistence", "Persisted model in " + FILE_NAME_NB_MODEL);

				// persist frc to file
				fos = getApplicationContext().openFileOutput(FILE_NAME_FRC, Context.MODE_PRIVATE);
				oos = new ObjectOutputStream(fos);
				oos.writeObject(frc);
				oos.close();
				Log.i("Persistence", "Persisted FRC in " + FILE_NAME_FRC);
				
				// TODO experimental
				startAccelerometerMonitoring(false);

			} else if (requestCode == REQUEST_CODE_ENABLE_BLUETOOTH) {
				Log.i("Bluetooth", "resultCode = " + resultCode);
				if (resultCode < 0) {
					Log.i("Bluetooth", "Bluetooth turned ON");
				} else {
					Log.i("Bluetooth", "Bluetooth still not turned ON");
				}
			}
		} catch (Exception e) {
			throw new RuntimeException(e);
		} finally {
			physicalActivityDialogueActivated = false;
			startAccelerometerMonitoring(false);
		}
	} 

	/*
	private void startAccelerometerMonitoring(boolean showToast) {
		Log.i("android state", "startAccelerometerMonitoring() called");
		//sampleWindow = new AccelerometerWindow();

		if (paused) {
			slidingWindow = new SlidingWindow(WINDOW_DURATION, true);
			sbd.start();
			SensorManager sensorManager = 
					(SensorManager) getSystemService(Context.SENSOR_SERVICE);
			Sensor accelerometer = 
					sensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER);
			sensorManager.registerListener(this, accelerometer, SAMPLING_DELAY);
			Log.i("Accelerometer", "Accelerometer started");
		} else {
			Log.i("Accelerometer", "Attempted to start accelerometer, but already running.");
		}

		paused = false;
		pauseResume.setText("Pause");
		pauseResume.setBackgroundColor(
				getResources().getColor(R.color.yellow));
		//pauseResume.setBackground(getResources().getDrawable(
		//		R.drawable.semi_rounded_left_pause));

		if (showToast) {
			Toast t = Toast.makeText(this, "Monitoring resumed", 
					Toast.LENGTH_SHORT);
			t.setGravity(Gravity.CENTER, 0, 0);
			t.show();
		}
	}
	 */
	private void startAccelerometerMonitoring(boolean showToast) {
		slidingWindowEnabled = true;
		sbd.start();
		paused = false;
		pauseResume.setText("Pause");
		pauseResume.setBackgroundColor(
				getResources().getColor(R.color.green));

		// onboard accelerometer
		SensorManager sensorManager = 
				(SensorManager) getSystemService(Context.SENSOR_SERVICE);
		Sensor accelerometer = 
				sensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER);
		sensorManager.registerListener(this, accelerometer, 
				SAMPLING_DELAY_ONBOARD);
	}

	/*
	private void stopAccelerometerMonitoring(boolean showToast) {
		Log.i("android state", "stopAccelerometerMonitoring() called");
		sbd.stop();
		SensorManager sensorManager = 
				(SensorManager) getSystemService(Context.SENSOR_SERVICE);
		Sensor accelerometer = sensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER);
		sensorManager.unregisterListener(this, accelerometer);
		Log.i("Accelerometer", "Accelerometer stopped");

		paused = true;
		pauseResume.setText("Resume");
		pauseResume.setBackgroundColor(
				getResources().getColor(R.color.green));
		//pauseResume.setBackground(getResources().getDrawable(
		//		R.drawable.semi_rounded_left_resume));

		if (show) {
			Toast t = Toast.makeText(this, "Monitoring paused", 
					Toast.LENGTH_SHORT);
			t.setGravity(Gravity.CENTER, 0, 0);
			t.show();
		}
	}
	 */
	private void stopAccelerometerMonitoring(boolean showToast) {
		Log.i("android state", "stopAccelerometerMonitoring() called");
		slidingWindowEnabled = false;
		slidingWindowDeviceOne = null;
		slidingWindowDeviceTwo = null;
		slidingWindowDeviceThree = null;
		slidingWindowDeviceFour = null;
		sbd.stop();
		paused = true;
		pauseResume.setText("Resume");
		pauseResume.setBackgroundColor(
				getResources().getColor(R.color.red));

		// onboard accelerometer
		SensorManager sensorManager = 
				(SensorManager) getSystemService(Context.SENSOR_SERVICE);
		Sensor accelerometer = sensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER);
		sensorManager.unregisterListener(this, accelerometer);

		// flush all onboard samples
		flushOnboardSamples();
		listSamplesOnboard = new LinkedList<AccelerometerSample>();
	}

	@Override
	public void onAccuracyChanged(Sensor sensor, int accuracy) {
	}

}

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
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import android.util.Log;

import weka.core.Instance;

public class SegmentBoundaryDetector implements Serializable {

	private static final long serialVersionUID = -8750471712905939663L;

	class SquareMatrix implements Serializable {
		private static final long serialVersionUID = 3588392223457732943L;
		protected double[][] m;
		protected int dim;

		public SquareMatrix(int dim) {
			this.dim = dim;
			m = new double[this.dim][this.dim];
		}

		public SquareMatrix multiplyElementWise(SquareMatrix rhs) {
			SquareMatrix result = new SquareMatrix(dim);
			for (int i = 0; i < dim; i++) {
				for (int k = 0; k < dim; k++) {
					result.m[i][k] = m[i][k] * rhs.m[i][k];
				}
			}
			return result;
		}

		public void set(int iRow, int iCol, double value) {
			m[iRow][iCol] = value;
		}

		public double get(int iRow, int iCol) {
			return m[iRow][iCol];
		}

		public double sum() {
			double s = 0;
			for (int i = 0; i < dim; i++) {
				for (int k = 0; k < dim; k++) {
					s += m[i][k];
				}
			}
			return s;
		}
	}

	class KernelMatrix extends SquareMatrix {
		private static final long serialVersionUID = 1900883626899403661L;

		public KernelMatrix(int L) {
			super(2*L);
			for (int i = 0; i < dim; i++) {
				for (int k = 0; k < dim; k++) {
					if (((i<L) & (k>=L)) | ((i>=L) & (k<L))) {
						m[i][k] = 1 / (double)(2*L*L);
					}
				}
			}
		}
	}

	List<AccelerometerFrameFourDevices> framesUnscaled = new LinkedList<AccelerometerFrameFourDevices>();
	List<Instance> instancesUnscaled = new LinkedList<Instance>();
	
	List<AccelerometerFrameFourDevices> framesScaled = new LinkedList<AccelerometerFrameFourDevices>();
	List<Instance> instancesScaled = new LinkedList<Instance>();

	List<List<Instance>> instancesUnscaledSegList = new LinkedList<List<Instance>>();
	List<List<AccelerometerFrameFourDevices>> framesUnscaledSegList = new LinkedList<List<AccelerometerFrameFourDevices>>();
	
	List<List<Instance>> instancesScaledSegList = new LinkedList<List<Instance>>();
	List<List<AccelerometerFrameFourDevices>> framesScaledSegList = new LinkedList<List<AccelerometerFrameFourDevices>>();

	int L;
	double th;
	List<SquareMatrix> distMatrices = new LinkedList<SquareMatrix>();
	List<Double> distances = new LinkedList<Double>();
	List<Double> decisionDistancesList = new LinkedList<Double>();
	KernelMatrix kernelMatrix;
	int iSegNow = -1;
	int iSegPrev = 0;
	boolean stopped = true;

	int numIgnoreFirstSegments;
	int numIgnoreSegmentsEitherSide;
	
	int iFramesSeen;
	
	double lastDistance;

	public SegmentBoundaryDetector(int kernelWidth, double threshold, 
			int numIgnoreFirstSegments, int numIgnoreSegmentsEitherSide) {
		this.L = kernelWidth;		
		kernelMatrix = new KernelMatrix(kernelWidth);

		this.th = threshold;

		this.numIgnoreFirstSegments = numIgnoreFirstSegments;
		this.numIgnoreSegmentsEitherSide = numIgnoreSegmentsEitherSide;
	}

	public void stop() {
		stopped = true;
	}

	public void start() {
		framesUnscaled = new LinkedList<AccelerometerFrameFourDevices>();
		instancesUnscaled = new LinkedList<Instance>();
		
		framesScaled = new LinkedList<AccelerometerFrameFourDevices>();
		instancesScaled = new LinkedList<Instance>();
		
		//segList = new LinkedList<List<AccelerometerWindow>>();
		distMatrices = new LinkedList<SquareMatrix>();
		distances = new LinkedList<Double>();

		decisionDistancesList = new LinkedList<Double>();
		
		instancesScaledSegList = new LinkedList<List<Instance>>();
		framesScaledSegList = new LinkedList<List<AccelerometerFrameFourDevices>>();
		
		instancesUnscaledSegList = new LinkedList<List<Instance>>();
		framesUnscaledSegList = new LinkedList<List<AccelerometerFrameFourDevices>>();

		iSegNow = -1;
		iSegPrev = 0;
		iFramesSeen = 0;
		stopped = false;
		
		lastDistance = 0f;
	}

	public void add(AccelerometerFrameFourDevices frame4dUnscaled, Instance instanceUnscaled,
			AccelerometerFrameFourDevices frame4dScaled, Instance instanceScaled) {
		// re-start detector
		if (stopped) {
			start();
		}
		if (++iFramesSeen <= numIgnoreFirstSegments) {
			//System.out.println("Dropping segment #" + iFramesSeen);
			Log.i("SBD", "Dropping segment #" + iFramesSeen);
			return;
		}

		//Log.w("Distance", "Added new frame");
		//Log.w("Distance", "size before = " + frames.size());

		framesUnscaled.add(frame4dUnscaled);
		instancesUnscaled.add(instanceUnscaled);
		
		framesScaled.add(frame4dScaled);
		instancesScaled.add(instanceScaled);

		Log.w("Distance", "size after  = " + framesScaled.size());

		if (framesScaled.size() >= 2*L) {
			int iEnd = framesScaled.size() - 1;
			int iStart = iEnd - 2*L + 1;

			SquareMatrix distMatrix = new SquareMatrix(2*L);
			if (distMatrices.size() > 0) {
				SquareMatrix prevMatrix = distMatrices.get(distMatrices.size()-1);
				// reuse 
				for (int i = 0; i < 2*L - 1; i++) {
					for (int k = 0; k < 2*L - 1; k++) {
						distMatrix.set(i, k, prevMatrix.get(i+1, k+1));
					}
				}
				// calculate last row and column
				int k = iEnd - 1;
				int kMatrix = k - iStart;
				for (int i = iStart; i < iEnd - 1; i++) {
					int iMatrix = i - iStart;


					double dist = distance(framesScaled.get(i), framesScaled.get(k));
					distMatrix.set(iMatrix, kMatrix, dist);
					distMatrix.set(kMatrix, iMatrix, dist);
				}
				// calculate lower right corner element
				double dist = distance(framesScaled.get(k), framesScaled.get(k));
				distMatrix.set(kMatrix, kMatrix, dist);
			} else {
				for (int i = iStart; i <= iEnd; i++) {
					for (int k = iStart; k <= iEnd; k++) {
						double dist = distance(framesScaled.get(i), framesScaled.get(k));
						int iMatrix = i - iStart;
						int kMatrix = k - iStart;
						distMatrix.set(iMatrix, kMatrix, dist);
					}
				}
			}
			distMatrices.add(distMatrix);

			double aggDist = distMatrix.multiplyElementWise(kernelMatrix).sum();
			lastDistance = aggDist;
			Log.i("Annotation", "Agg dist = " + aggDist);
			distances.add(aggDist);

			if (distances.size() >= 3) {
				int iLastDist = distances.size() - 1;
				double dist1 = distances.get(iLastDist - 2);
				double dist2 = distances.get(iLastDist - 1);
				double dist3 = distances.get(iLastDist);
				if ((dist2 >= dist1) & (dist2 >= dist3) & (dist2 > th)) {
					iSegNow = framesScaled.size() - L - 2;
					//iSegNow = framesScaled.size() - L - 2 - numIgnoreSegmentsEitherSide;

					int iSegNowMinusEitherSide = iSegNow - numIgnoreSegmentsEitherSide;
					int iSegPrevPlusEitherSide = iSegPrev + numIgnoreSegmentsEitherSide;
					
					if (iSegPrevPlusEitherSide > iSegNowMinusEitherSide) {
						Log.i("SBD", "iSegPrev=" + iSegPrevPlusEitherSide + 
								"; iSegNow=" + iSegNowMinusEitherSide + "; segment too short");
						
						iSegPrev = iSegNow++;
						
						return;
					}
					
					//List<Instance> instancesScaledSegment = new ArrayList<Instance>(instancesScaled.subList(iSegPrev, iSegNow + 1));
					List<Instance> instancesScaledSegment = new ArrayList<Instance>(
							instancesScaled.subList(iSegPrevPlusEitherSide, iSegNowMinusEitherSide + 1));
					instancesScaledSegList.add(instancesScaledSegment);

					//List<AccelerometerFrameFourDevices> framesScaledSegment = new ArrayList<AccelerometerFrameFourDevices>(
					//		framesScaled.subList(iSegPrev, iSegNow + 1));
					List<AccelerometerFrameFourDevices> framesScaledSegment = new ArrayList<AccelerometerFrameFourDevices>(
							framesScaled.subList(iSegPrevPlusEitherSide, iSegNowMinusEitherSide + 1));
					framesScaledSegList.add(framesScaledSegment);
					
					//List<Instance> instancesUnscaledSegment = new ArrayList<Instance>(instancesUnscaled.subList(iSegPrev, iSegNow + 1));
					List<Instance> instancesUnscaledSegment = new ArrayList<Instance>(
							instancesUnscaled.subList(iSegPrevPlusEitherSide, iSegNowMinusEitherSide + 1));
					instancesUnscaledSegList.add(instancesUnscaledSegment);

					//List<AccelerometerFrameFourDevices> framesUnscaledSegment = new ArrayList<AccelerometerFrameFourDevices>(
					//		framesUnscaled.subList(iSegPrev, iSegNow + 1));
					List<AccelerometerFrameFourDevices> framesUnscaledSegment = new ArrayList<AccelerometerFrameFourDevices>(
							framesUnscaled.subList(iSegPrevPlusEitherSide, iSegNowMinusEitherSide + 1));
					framesUnscaledSegList.add(framesUnscaledSegment);
					
					decisionDistancesList.add(dist2);
					
					Log.i("SBD", "iSegNow = " + iSegNow);
					Log.i("SBD", "iSegPrev = " + iSegPrev);
					Log.i("SBD", "Seg indices = [" + iSegPrevPlusEitherSide + ", " + (iSegNowMinusEitherSide + 1) + "]");
					Log.i("SBD", "Num frames in segment = " + framesUnscaledSegment.size());
					
					//List<AccelerometerFrameFourDevices> scaledFramesSegment = new ArrayList<AccelerometerFrameFourDevices>();

					//Log.w("Distance", "--- Local maximum detected");
					//Log.w("Distance", "\t iSegPrev = " + iSegPrev);
					//Log.w("Distance", "\t iSegnNow  = " + iSegNow);
					//Log.w("Distance", "\t iLastDist = " + iLastDist);			
					//Log.w("Distance", "\t Seg size  = " + segment.size());
					iSegPrev = iSegNow++;
				}
			}
		}
	}

	private double distance(AccelerometerFrameFourDevices frame1, AccelerometerFrameFourDevices frame2) {
		// device one
		double sqMeanXDeviceOne = Math.pow(frame1.getMeanXDeviceOne() - frame2.getMeanXDeviceOne(), 2);
		double sqMeanYDeviceOne = Math.pow(frame1.getMeanYDeviceOne() - frame2.getMeanYDeviceOne(), 2);
		double sqMeanZDeviceOne = Math.pow(frame1.getMeanZDeviceOne() - frame2.getMeanZDeviceOne(), 2);		
		double sqVarXDeviceOne = Math.pow(frame1.getVarXDeviceOne() - frame2.getVarXDeviceOne(), 2);
		double sqVarYDeviceOne = Math.pow(frame1.getVarYDeviceOne() - frame2.getVarYDeviceOne(), 2);
		double sqVarZDeviceOne = Math.pow(frame1.getVarZDeviceOne() - frame2.getVarZDeviceOne(), 2);		
		double sqCorXYDeviceOne = Math.pow(frame1.getCorXYDeviceOne() - frame2.getCorXYDeviceOne(), 2);
		double sqCorYZDeviceOne = Math.pow(frame1.getCorYZDeviceOne() - frame2.getCorYZDeviceOne(), 2);
		double sqCorZXDeviceOne = Math.pow(frame1.getCorZXDeviceOne() - frame2.getCorZXDeviceOne(), 2);

		// device two
		double sqMeanXDeviceTwo = Math.pow(frame1.getMeanXDeviceTwo() - frame2.getMeanXDeviceTwo(), 2);
		double sqMeanYDeviceTwo = Math.pow(frame1.getMeanYDeviceTwo() - frame2.getMeanYDeviceTwo(), 2);
		double sqMeanZDeviceTwo = Math.pow(frame1.getMeanZDeviceTwo() - frame2.getMeanZDeviceTwo(), 2);		
		double sqVarXDeviceTwo = Math.pow(frame1.getVarXDeviceTwo() - frame2.getVarXDeviceTwo(), 2);
		double sqVarYDeviceTwo = Math.pow(frame1.getVarYDeviceTwo() - frame2.getVarYDeviceTwo(), 2);
		double sqVarZDeviceTwo = Math.pow(frame1.getVarZDeviceTwo() - frame2.getVarZDeviceTwo(), 2);		
		double sqCorXYDeviceTwo = Math.pow(frame1.getCorXYDeviceTwo() - frame2.getCorXYDeviceTwo(), 2);
		double sqCorYZDeviceTwo = Math.pow(frame1.getCorYZDeviceTwo() - frame2.getCorYZDeviceTwo(), 2);
		double sqCorZXDeviceTwo = Math.pow(frame1.getCorZXDeviceTwo() - frame2.getCorZXDeviceTwo(), 2);

		// device three
		double sqMeanXDeviceThree = Math.pow(frame1.getMeanXDeviceThree() - frame2.getMeanXDeviceThree(), 2);
		double sqMeanYDeviceThree = Math.pow(frame1.getMeanYDeviceThree() - frame2.getMeanYDeviceThree(), 2);
		double sqMeanZDeviceThree = Math.pow(frame1.getMeanZDeviceThree() - frame2.getMeanZDeviceThree(), 2);		
		double sqVarXDeviceThree = Math.pow(frame1.getVarXDeviceThree() - frame2.getVarXDeviceThree(), 2);
		double sqVarYDeviceThree = Math.pow(frame1.getVarYDeviceThree() - frame2.getVarYDeviceThree(), 2);
		double sqVarZDeviceThree = Math.pow(frame1.getVarZDeviceThree() - frame2.getVarZDeviceThree(), 2);		
		double sqCorXYDeviceThree = Math.pow(frame1.getCorXYDeviceThree() - frame2.getCorXYDeviceThree(), 2);
		double sqCorYZDeviceThree = Math.pow(frame1.getCorYZDeviceThree() - frame2.getCorYZDeviceThree(), 2);
		double sqCorZXDeviceThree = Math.pow(frame1.getCorZXDeviceThree() - frame2.getCorZXDeviceThree(), 2);

		// device four
		double sqMeanXDeviceFour = Math.pow(frame1.getMeanXDeviceFour() - frame2.getMeanXDeviceFour(), 2);
		double sqMeanYDeviceFour = Math.pow(frame1.getMeanYDeviceFour() - frame2.getMeanYDeviceFour(), 2);
		double sqMeanZDeviceFour = Math.pow(frame1.getMeanZDeviceFour() - frame2.getMeanZDeviceFour(), 2);		
		double sqVarXDeviceFour = Math.pow(frame1.getVarXDeviceFour() - frame2.getVarXDeviceFour(), 2);
		double sqVarYDeviceFour = Math.pow(frame1.getVarYDeviceFour() - frame2.getVarYDeviceFour(), 2);
		double sqVarZDeviceFour = Math.pow(frame1.getVarZDeviceFour() - frame2.getVarZDeviceFour(), 2);		
		double sqCorXYDeviceFour = Math.pow(frame1.getCorXYDeviceFour() - frame2.getCorXYDeviceFour(), 2);
		double sqCorYZDeviceFour = Math.pow(frame1.getCorYZDeviceFour() - frame2.getCorYZDeviceFour(), 2);
		double sqCorZXDeviceFour = Math.pow(frame1.getCorZXDeviceFour() - frame2.getCorZXDeviceFour(), 2);

		double sumSq = sqMeanXDeviceOne + sqMeanYDeviceOne + sqMeanZDeviceOne + 
				sqVarXDeviceOne + sqVarYDeviceOne + sqVarZDeviceOne +
				sqCorXYDeviceOne + sqCorYZDeviceOne + sqCorZXDeviceOne +

				sqMeanXDeviceTwo + sqMeanYDeviceTwo + sqMeanZDeviceTwo + 
				sqVarXDeviceTwo + sqVarYDeviceTwo + sqVarZDeviceTwo +
				sqCorXYDeviceTwo + sqCorYZDeviceTwo + sqCorZXDeviceTwo + 

				sqMeanXDeviceThree + sqMeanYDeviceThree + sqMeanZDeviceThree + 
				sqVarXDeviceThree + sqVarYDeviceThree + sqVarZDeviceThree +
				sqCorXYDeviceThree + sqCorYZDeviceThree + sqCorZXDeviceThree +

				sqMeanXDeviceFour + sqMeanYDeviceFour + sqMeanZDeviceFour + 
				sqVarXDeviceFour + sqVarYDeviceFour + sqVarZDeviceFour +
				sqCorXYDeviceFour + sqCorYZDeviceFour + sqCorZXDeviceFour;
		
		// scaling factor is sqrt(36). 
		// 36 features in [0, 1]
		double dist = Math.sqrt(sumSq / 36);

		//Log.i("Annotation", "Raw distance = " + String.format("%.3f", dist));

		return dist;
	}

	public double getDecisionDistance() {
		if (decisionDistancesList.size() > 0) {
			return decisionDistancesList.get(decisionDistancesList.size() - 1); 
		} else {
			return 0f;
		}
	}
	
	public double getLastDistance() {
		return lastDistance;
	}
	
	public boolean segmentBoundaryDetected() {
		return instancesUnscaledSegList.size() > 0;
	}

	public List<Instance> getWekaInstancesInSegment() {
		if (instancesUnscaledSegList.size() > 0) {
			List<Instance> segment = instancesUnscaledSegList.get(0);
			//instancesSegList = new LinkedList<List<Instance>>();
			stop();
			return segment; 
		} else {
			return null;
		}
	}

	public List<AccelerometerFrameFourDevices> getFramesInSegment() {
		if (framesUnscaledSegList.size() > 0) {
			List<AccelerometerFrameFourDevices> segment = framesUnscaledSegList.get(0);
			//framesSegList = new LinkedList<List<AccelerometerFrame>>();
			stop();
			return segment;
		} else {
			return null;
		}
	}
	
	public int numFramesInBuffer() {
		return framesUnscaled.size();
	}
	
	public void forceSegmentBoundaryNow() {
		int rangeIndexStart = numIgnoreSegmentsEitherSide - 1;
		int rangeIndexEnd = framesUnscaled.size() - 1 - L - numIgnoreSegmentsEitherSide;
		List<Instance> instancesScaledSegment = new ArrayList<Instance>(
				instancesScaled.subList(rangeIndexStart, rangeIndexEnd));
		instancesScaledSegList.add(instancesScaledSegment);

		List<AccelerometerFrameFourDevices> framesScaledSegment = new ArrayList<AccelerometerFrameFourDevices>(
				framesScaled.subList(rangeIndexStart, rangeIndexEnd));
		framesScaledSegList.add(framesScaledSegment);
		
		List<Instance> instancesUnscaledSegment = new ArrayList<Instance>(
				instancesUnscaled.subList(rangeIndexStart, rangeIndexEnd));
		instancesUnscaledSegList.add(instancesUnscaledSegment);

		List<AccelerometerFrameFourDevices> framesUnscaledSegment = new ArrayList<AccelerometerFrameFourDevices>(
				framesUnscaled.subList(rangeIndexStart, rangeIndexEnd));
		framesUnscaledSegList.add(framesUnscaledSegment);
		
		decisionDistancesList.add(Double.valueOf(0f));
	}

	//	private AccelerometerSample generateRandomReading(Random rand) {		
	//		return new AccelerometerSample(rand.nextDouble(), 
	//				rand.nextDouble(), rand.nextDouble(), rand.nextLong());
	//	}
	//	
	//	private AccelerometerWindow generateRandomFrame(Random rand) {
	//		AccelerometerWindow frame = new AccelerometerWindow();
	//		for (int i=0; i < 100; i++) {
	//			frame.addSample(generateRandomReading(rand));
	//		}
	//		frame.finalizeFeatures();
	//		return frame;
	//	}
	//	
	//	public static void main(String[] args) {
	//		/*
	//		int kernelWidth = 3;
	//		double threshold = 0.4;
	//		int numIgnoreFirstSegments = 4;
	//		SegmentBoundaryDetector sbd = new SegmentBoundaryDetector(kernelWidth, threshold, numIgnoreFirstSegments);
	//		
	//		Random rand = new Random();
	//		for (int i = 0; i < 200; i++) {
	//			sbd.add(sbd.generateRandomFrame(rand));
	//			
	//			if (sbd.segmentBoundaryDetected()) {
	//				System.out.printf("%4d: segment detected\n", i);
	//				sbd.getSegment();
	//			}
	//		}
	//		
	//		System.out.println("Done!");
	//		*/
	//	}

}


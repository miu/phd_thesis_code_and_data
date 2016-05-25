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
import java.util.Iterator;
import java.util.LinkedList;
import org.apache.commons.math3.stat.correlation.Covariance;
import org.apache.commons.math3.stat.correlation.PearsonsCorrelation;
import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics;

public class AccelerometerFrame implements Serializable {

	private static final long serialVersionUID = 6669254342594784079L;

	private static final int STATS_WINDOW_LENGTH = 1000;

	private DescriptiveStatistics xStats = new DescriptiveStatistics(STATS_WINDOW_LENGTH);	
	private DescriptiveStatistics yStats = new DescriptiveStatistics(STATS_WINDOW_LENGTH);	
	private DescriptiveStatistics zStats = new DescriptiveStatistics(STATS_WINDOW_LENGTH);
	private boolean isFilled = false;

	private double meanX;
	private double meanY;
	private double meanZ;
	private double varX;
	private double varY;
	private double varZ;
	private double corXY;
	private double corYZ;
	private double corZX;

	private LinkedList<Long> t = new LinkedList<Long>();
	
	private long tStart;
	private long tEnd;
	
	private long tFirst;
	private long tLast;
	
	private int size;
	
	public AccelerometerFrame(long tStart, long tEnd) {
		this.tStart = tStart;
		this.tEnd = tEnd;
	}

	public void add(AccelerometerSample sample) {
		xStats.addValue(sample.getX());
		yStats.addValue(sample.getY());
		zStats.addValue(sample.getZ());

		t.addLast(sample.getT());
	}

	public long getStartTimestamp() { return tStart; }
	public long getEndTimestamp() { return tEnd; }
	
	public long getFirstTimestamp() { 
		if (t != null) {
			tFirst = t.getFirst();
		}
		return tFirst; 
	}	
	
	public long getLastTimestamp() { 
		if (t != null) {
			tLast = t.getLast();
		}
		return tLast;
	}
	
	public int getSize() { 
		if (t != null) {
			size = t.size();
		}		
		return size;
	}

	public void computeFeatures() {
		meanX = xStats.getMean();
		meanY = yStats.getMean();
		meanZ = zStats.getMean();

		varX = xStats.getVariance();
		varY = yStats.getVariance();
		varZ = zStats.getVariance();

		double[] xValues = xStats.getValues();
		double[] yValues = yStats.getValues();
		double[] zValues = zStats.getValues();
		corXY = new PearsonsCorrelation().correlation(xValues, yValues);
		corYZ = new PearsonsCorrelation().correlation(yValues, zValues);
		corZX = new PearsonsCorrelation().correlation(zValues, xValues);
	}

	public double getMeanX() throws FrameNotFilledException {
		if (!isFilled) {
			throw new FrameNotFilledException();
		}
		return meanX; 
	}	
	public double getMeanY() throws FrameNotFilledException {
		if (!isFilled) {
			throw new FrameNotFilledException();
		}
		return meanY; 
	}	
	public double getMeanZ() throws FrameNotFilledException {
		if (!isFilled) {
			throw new FrameNotFilledException();
		}
		return meanZ; 
	}	

	public void setMeanX(double meanX) { this.meanX = meanX; }
	public void setMeanY(double meanY) { this.meanY = meanY; }
	public void setMeanZ(double meanZ) { this.meanZ = meanZ; }
	
	public void setT(long[] t) {
		this.t = new LinkedList<Long>();
		for (long time : t) {
			this.t.addLast(time);
		}
	}

	public double getVarX() throws FrameNotFilledException {
		if (!isFilled) {
			throw new FrameNotFilledException();
		} 
		return varX; 
	}
	public double getVarY() throws FrameNotFilledException {
		if (!isFilled) {
			throw new FrameNotFilledException();
		} 
		return varY; 
	}
	public double getVarZ() throws FrameNotFilledException {
		if (!isFilled) {
			throw new FrameNotFilledException();
		} 
		return varZ; 
	}
	public void setVarX(double varX) { this.varX = varX; }
	public void setVarY(double varY) { this.varY = varY; }
	public void setVarZ(double varZ) { this.varZ = varZ; }

	public double getCorXY() throws FrameNotFilledException {
		if (!isFilled) {
			throw new FrameNotFilledException();
		}
		return corXY; 
	}
	public double getCorYZ() throws FrameNotFilledException {
		if (!isFilled) {
			throw new FrameNotFilledException();
		}
		return corYZ; 
	}
	public double getCorZX() throws FrameNotFilledException {
		if (!isFilled) {
			throw new FrameNotFilledException();
		} 
		return corZX; 
	}
	public void setCorXY(double corXY) { this.corXY = corXY; }
	public void setCorYZ(double corYZ) { this.corYZ = corYZ; }
	public void setCorZX(double corZX) { this.corZX = corZX; }

	public void setFilled(boolean computeFeatures) {
		isFilled = true;

		if (computeFeatures) {
			computeFeatures();
		}
	}

	public boolean getFilled() { return isFilled; }
	
	public double[] getX() { return xStats.getValues(); }
	public double[] getY() { return yStats.getValues(); }
	public double[] getZ() { return zStats.getValues(); }
	
	public long[] getT() { 
		if (t == null) {
			return null;
		}
		int size = t.size();
		long[] v = new long[size];
		Iterator<Long> it = t.iterator();
		for (int i = 0; i < size; i++) {
			v[i] = it.next().longValue();
		}
		return v;
	}
	
	public void dropSignalData() {
		xStats = null;
		yStats = null;
		zStats = null;
		t = null;
		isFilled = false;
	}
	
	public AccelerometerFrame getCopyWithoutSignalData() {
		AccelerometerFrame afCopy = new AccelerometerFrame(tStart, tEnd);
		
		afCopy.meanX = meanX;
		afCopy.meanY = meanY;
		afCopy.meanZ = meanZ;
		
		afCopy.varX = varX;
		afCopy.varY = varY;
		afCopy.varZ = varZ;
		
		afCopy.corXY = corXY;
		afCopy.corYZ = corYZ;
		afCopy.corZX = corZX;
		
		afCopy.isFilled = isFilled;
		
		afCopy.size = size;
		
		afCopy.t = null;
		afCopy.xStats = null;
		afCopy.yStats = null;
		afCopy.zStats = null;
		
		return afCopy;
	}
}

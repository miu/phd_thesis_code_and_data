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

import java.io.Serializable;

import weka.core.Instance;

public class InstancePOJO implements Serializable {
	
	private static final long serialVersionUID = 7946002363433747319L;
	
	private Instance instance;
	private int segmentNumber;
	long startTimestamp;
	long endTimestamp;
	
	public InstancePOJO(Instance instance, int segmentNumber, long startTimestamp, long endTimestamp) {
		this.instance = instance;
		this.segmentNumber = segmentNumber;
		this.startTimestamp = startTimestamp;
		this.endTimestamp = endTimestamp;
	}
	
	public Instance getInstance() {
		return instance;
	}

	public int getSegmentNumber() {
		return segmentNumber;
	}

	public long getStartTimestamp() {
		return startTimestamp;
	}

	public long getEndTimestamp() {
		return endTimestamp;
	}
}

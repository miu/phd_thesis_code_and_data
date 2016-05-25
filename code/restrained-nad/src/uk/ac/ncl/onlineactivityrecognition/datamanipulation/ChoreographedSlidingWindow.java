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

public class ChoreographedSlidingWindow implements Serializable {

	private static final long serialVersionUID = -5441421182680463966L;

	
	private long length;
	private long tStart;
	private long tMidpoint;
	
	private final boolean overlap = true;

	private LinkedList<AccelerometerFrame> frames = new LinkedList<AccelerometerFrame>();

	public ChoreographedSlidingWindow(long tStart, long length) {
		this.tStart = tStart;
		this.length = length;
		
		this.tMidpoint = tStart + this.length/2;
	}

	public boolean add(AccelerometerSample sample) {
		AccelerometerFrame lastFrame = getLastFrame();
		AccelerometerFrame prevFrame = getPrevFrame();

		boolean frameFilled = false;

		if (lastFrame == null) {
			lastFrame = new AccelerometerFrame(tStart, tStart + length - 1);
			frames.addFirst(lastFrame);
		}

		if (prevFrame == null) {
			prevFrame = new AccelerometerFrame(tMidpoint, tMidpoint + length - 1);
			frames.addLast(prevFrame);
		}

		if (lastFrame.getSize() > 0) {
			//long firstTimestamp = lastFrame.getFirstTimestamp();
			long tCurrentReading = sample.getT();
			long timeDiff = tCurrentReading - tStart;

			// add to prev as long as window length not exceeded
			if (timeDiff <= length) {
				lastFrame.add(sample);
			}

			if (overlap) {
				// add to last (newest) frame if 50% mark surpassed
				if ( (timeDiff <= length) && (timeDiff > (length/2)) ) {
					prevFrame.add(sample);
				}
			}
			// if current window is exceeded, then switch windows
			if (timeDiff > length) {
				lastFrame.setFilled(true);
				frameFilled = true;

				tStart = tMidpoint;
				tMidpoint += length / 2;
				
				lastFrame = prevFrame;
				prevFrame = new AccelerometerFrame(tMidpoint, tMidpoint + length - 1);
				frames.addLast(prevFrame);

				lastFrame.add(sample);
			}
		} else {
			lastFrame.add(sample);
		}

		return frameFilled;
	}

	public AccelerometerFrame popFrame() {
		AccelerometerFrame frame = frames.peekFirst();
		if (frame.getFilled()) {
			frame = frames.removeFirst();
			return frame;
		} else {
			return null;
		}
	}

	public boolean isFrameCompleted() {
		if (frames.size() > 0) {
			AccelerometerFrame frame = frames.peekFirst();
			return frame.getFilled();
		} else {
			return false;
		}
	}
	
	private AccelerometerFrame getLastFrame() {
		if (frames.size() > 0) {
			return frames.getFirst();
		} else {
			return null;
		}
	}

	private AccelerometerFrame getPrevFrame() {
		if (frames.size() > 1) {
			return frames.get(1);
		} else {
			return null;
		}
	}
}

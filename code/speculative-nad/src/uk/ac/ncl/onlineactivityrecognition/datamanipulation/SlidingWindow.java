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

public class SlidingWindow implements Serializable {

	private static final long serialVersionUID = -5441421182680463966L;
	/*
	private long length;
	private boolean overlap;

	private LinkedList<AccelerometerFrame> frames = new LinkedList<AccelerometerFrame>();

	public SlidingWindow(long length, boolean overlap) {
		this.length = length;
		this.overlap = overlap;
	}

	public boolean add(AccelerometerSample sample) {
		AccelerometerFrame lastFrame = getLastFrame();
		AccelerometerFrame prevFrame = getPrevFrame();

		boolean frameFilled = false;

		if (lastFrame == null) {
			lastFrame = new AccelerometerFrame();
			frames.addFirst(lastFrame);
		}

		if (prevFrame == null) {
			prevFrame = new AccelerometerFrame();
			frames.addLast(prevFrame);
		}

		if (lastFrame.getSize() > 0) {
			long firstTimestamp = lastFrame.getFirstTimestamp();
			long currentTimestamp = sample.getT();
			long timeDiff = currentTimestamp - firstTimestamp;

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

				lastFrame = prevFrame;
				prevFrame = new AccelerometerFrameOnboard();
				frames.addLast(prevFrame);

				lastFrame.add(sample);
			}
		} else {
			lastFrame.add(sample);
		}

		return frameFilled;
	}

	public AccelerometerFrameOnboard popFrame() {
		AccelerometerFrameOnboard frame = frames.peekFirst();
		if (frame.getFilled()) {
			frame = frames.removeFirst();
			return frame;
		} else {
			return null;
		}
	}

	private AccelerometerFrameOnboard getLastFrame() {
		if (frames.size() > 0) {
			return frames.getFirst();
		} else {
			return null;
		}
	}

	private AccelerometerFrameOnboard getPrevFrame() {
		if (frames.size() > 1) {
			return frames.get(1);
		} else {
			return null;
		}
	}
	*/
}

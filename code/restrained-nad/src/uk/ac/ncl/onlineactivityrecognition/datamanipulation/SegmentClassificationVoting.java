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

import java.util.LinkedList;
import java.util.Random;

public class SegmentClassificationVoting {
	
	int[] votes;
	int listIndex;
	
	public SegmentClassificationVoting(int numClasses) {
		votes = new int[numClasses];
	}
	
	public void addVote(int i) {
		votes[i]++;
	}
	
	public int getMajorityVote() {
		int voteCount = votes[0];
		
		for (int i = 1; i < votes.length; i++) {
			voteCount = Math.max(voteCount, votes[i]);
		}
		
		LinkedList<Integer> ties = new LinkedList<Integer>();
		for (int i = 0; i < votes.length; i++) {
			if (votes[i] == voteCount) {
				ties.add(i);
			}
		}
		
		listIndex = new Random().nextInt(ties.size()); 
		return ties.get(listIndex);
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < votes.length; i++) {
			if (i == listIndex) {
				sb.append("*");
				sb.append(votes[listIndex]);
				sb.append("* ");
			} else {
				sb.append(votes[i]);
				sb.append(" ");
			}
		}
		return sb.toString();
	}
}

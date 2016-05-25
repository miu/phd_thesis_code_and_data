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

import java.util.Timer;
import java.util.TimerTask;

import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.os.PowerManager;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import android.widget.Button;

public class LabelCollectionActivity extends Activity {

	Button btnWalking;
	Button btnRunning;
	
	Button btnSitting;	
	Button btnStanding;
	Button btnSittingKneeRaises;
	
	Button btnCalfRaises;
	Button btnSquats;
	
	Button btnTorsoSideLeans;
	Button btnTorsoForwardBackward;
	Button btnTorsoTwists;
	
	Button btnDiscard;
	
	class ButtonListener implements View.OnClickListener {
		int labelIndex;
		
		public ButtonListener(int labelIndex) {
			this.labelIndex = labelIndex; 
		}
		
		@Override
		public void onClick(View v) {
			setResult(labelIndex);
			Log.i("LCA", "returning label");
			finish();
		}
	};
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		
		Log.i("Screen Locking", "LabelCollectionActivity.onCreate()");
		
		Window window = getWindow();
		window.addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
		window.addFlags(WindowManager.LayoutParams.FLAG_DISMISS_KEYGUARD);
		window.addFlags(WindowManager.LayoutParams.FLAG_SHOW_WHEN_LOCKED);
		window.addFlags(WindowManager.LayoutParams.FLAG_TURN_SCREEN_ON);
		
		setContentView(R.layout.activity_label_collection);
		
		btnWalking = (Button) findViewById(R.id.walking);
		btnWalking.setOnClickListener(new ButtonListener(0));
		
		//btnRunning = (Button) findViewById(R.id.running);
		//btnRunning.setOnClickListener(new ButtonListener(1));
		
		
		btnSitting = (Button) findViewById(R.id.sitting);
		btnSitting.setOnClickListener(new ButtonListener(1));
		
		btnStanding = (Button) findViewById(R.id.standing);
		btnStanding.setOnClickListener(new ButtonListener(2));
		
		btnSittingKneeRaises = (Button) findViewById(R.id.sitting_knee_raises);
		btnSittingKneeRaises.setOnClickListener(new ButtonListener(3));
		
		
		btnCalfRaises = (Button) findViewById(R.id.calf_raises);
		btnCalfRaises.setOnClickListener(new ButtonListener(4));
		
		btnSquats = (Button) findViewById(R.id.squats);
		btnSquats.setOnClickListener(new ButtonListener(5));
		
		
		btnTorsoSideLeans = (Button) findViewById(R.id.torso_side_to_side);
		btnTorsoSideLeans.setOnClickListener(new ButtonListener(6));
		
		btnTorsoForwardBackward = (Button) findViewById(R.id.torso_forward_backward);
		btnTorsoForwardBackward.setOnClickListener(new ButtonListener(7));
		
		btnTorsoTwists = (Button) findViewById(R.id.torso_twists);
		btnTorsoTwists.setOnClickListener(new ButtonListener(8));
		
		
		btnDiscard = (Button) findViewById(R.id.discard);
		btnDiscard.setOnClickListener(new ButtonListener(-1));
		
		// discard activity activity after some time
		TimerTask timerTask = new TimerTask() {
			@Override
			public void run() {
				setResult(-1);
				finish();
			}			
		};
		Timer timer = new Timer();
		timer.schedule(timerTask, 15000);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.activity_label_collection, menu);
		return true;
	}

}

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

import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;
import android.content.Context;;

public class ARSQLiteHelper extends SQLiteOpenHelper {

	private final static String DATABASE_NAME = "activity_recognition.db";
	private final static int DATABASE_VERSION = 1;

	final static String TABLE_INSTANCES = "instances";
	final static String INSTANCES_COLUMN_ID = "_id";	

	final static String INSTANCES_COLUMN_MEAN_X_DEVICE_ONE = "mean_x_device_one";
	final static String INSTANCES_COLUMN_MEAN_Y_DEVICE_ONE = "mean_y_device_one";
	final static String INSTANCES_COLUMN_MEAN_Z_DEVICE_ONE = "mean_z_device_one";	
	final static String INSTANCES_COLUMN_VAR_X_DEVICE_ONE = "var_x_device_one";
	final static String INSTANCES_COLUMN_VAR_Y_DEVICE_ONE = "var_y_device_one";
	final static String INSTANCES_COLUMN_VAR_Z_DEVICE_ONE = "var_z_device_one";	
	final static String INSTANCES_COLUMN_COR_XY_DEVICE_ONE = "cor_xy_device_one";
	final static String INSTANCES_COLUMN_COR_YZ_DEVICE_ONE = "cor_yz_device_one";
	final static String INSTANCES_COLUMN_COR_ZX_DEVICE_ONE = "cor_zx_device_one";

	final static String INSTANCES_COLUMN_MEAN_X_DEVICE_TWO = "mean_x_device_two";
	final static String INSTANCES_COLUMN_MEAN_Y_DEVICE_TWO = "mean_y_device_two";
	final static String INSTANCES_COLUMN_MEAN_Z_DEVICE_TWO = "mean_z_device_two";	
	final static String INSTANCES_COLUMN_VAR_X_DEVICE_TWO = "var_x_device_two";
	final static String INSTANCES_COLUMN_VAR_Y_DEVICE_TWO = "var_y_device_two";
	final static String INSTANCES_COLUMN_VAR_Z_DEVICE_TWO = "var_z_device_two";	
	final static String INSTANCES_COLUMN_COR_XY_DEVICE_TWO = "cor_xy_device_two";
	final static String INSTANCES_COLUMN_COR_YZ_DEVICE_TWO = "cor_yz_device_two";
	final static String INSTANCES_COLUMN_COR_ZX_DEVICE_TWO = "cor_zx_device_two";

	final static String INSTANCES_COLUMN_MEAN_X_DEVICE_THREE = "mean_x_device_three";
	final static String INSTANCES_COLUMN_MEAN_Y_DEVICE_THREE = "mean_y_device_three";
	final static String INSTANCES_COLUMN_MEAN_Z_DEVICE_THREE = "mean_z_device_three";	
	final static String INSTANCES_COLUMN_VAR_X_DEVICE_THREE = "var_x_device_three";
	final static String INSTANCES_COLUMN_VAR_Y_DEVICE_THREE = "var_y_device_three";
	final static String INSTANCES_COLUMN_VAR_Z_DEVICE_THREE = "var_z_device_three";	
	final static String INSTANCES_COLUMN_COR_XY_DEVICE_THREE = "cor_xy_device_three";
	final static String INSTANCES_COLUMN_COR_YZ_DEVICE_THREE = "cor_yz_device_three";
	final static String INSTANCES_COLUMN_COR_ZX_DEVICE_THREE = "cor_zx_device_three";

	final static String INSTANCES_COLUMN_MEAN_X_DEVICE_FOUR = "mean_x_device_four";
	final static String INSTANCES_COLUMN_MEAN_Y_DEVICE_FOUR = "mean_y_device_four";
	final static String INSTANCES_COLUMN_MEAN_Z_DEVICE_FOUR = "mean_z_device_four";	
	final static String INSTANCES_COLUMN_VAR_X_DEVICE_FOUR = "var_x_device_four";
	final static String INSTANCES_COLUMN_VAR_Y_DEVICE_FOUR = "var_y_device_four";
	final static String INSTANCES_COLUMN_VAR_Z_DEVICE_FOUR = "var_z_device_four";	
	final static String INSTANCES_COLUMN_COR_XY_DEVICE_FOUR = "cor_xy_device_four";
	final static String INSTANCES_COLUMN_COR_YZ_DEVICE_FOUR = "cor_yz_device_four";
	final static String INSTANCES_COLUMN_COR_ZX_DEVICE_FOUR = "cor_zx_device_four";

	final static String INSTANCES_COLUMN_T_START = "t_start";
	final static String INSTANCES_COLUMN_T_END = "t_end";
	final static String INSTANCES_COLUMN_SEGMENT_NUMBER = "segment_number";
	final static String INSTANCES_COLUMN_ACTIVITY = "activity";

	final static String TIMESTAMP_COLUMN = "timestamp";
	final static String SMALL_CONFIDENCE_COLUMN = "small_confidence";
	final static String BIG_CONFIDENCE_COLUMN = "big_confidence";
	
	final static String TABLE_UNLABELLED_INSTANCES = "unlabelled_instances";
	final static String TABLE_CLUSTER_INSTANCES = "cluster_label_instances";

	final static String TABLE_READINGS_DEVICE_ONE = "readings_device_one";
	final static String TABLE_READINGS_DEVICE_TWO = "readings_device_two";
	final static String TABLE_READINGS_DEVICE_THREE = "readings_device_three";
	final static String TABLE_READINGS_DEVICE_FOUR = "readings_device_four";
	
	final static String READINGS_COLUMN_ID = "_id";
	final static String READINGS_COLUMN_T = "t";
	final static String READINGS_COLUMN_X = "x";
	final static String READINGS_COLUMN_Y = "y";
	final static String READINGS_COLUMN_Z = "z";	

	private static final String[] DATABASE_CREATE = { 
		// instances table
		"create table " + TABLE_INSTANCES + "("
				+ INSTANCES_COLUMN_ID + " integer primary key autoincrement, "

		+ INSTANCES_COLUMN_MEAN_X_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Y_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Z_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_VAR_X_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_VAR_Y_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_VAR_Z_DEVICE_ONE + " real not null ,"
		+ INSTANCES_COLUMN_COR_XY_DEVICE_ONE + " real not null ,"
		+ INSTANCES_COLUMN_COR_YZ_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_COR_ZX_DEVICE_ONE + " real not null, "

		+ INSTANCES_COLUMN_MEAN_X_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Y_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Z_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_VAR_X_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_VAR_Y_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_VAR_Z_DEVICE_TWO + " real not null ,"
		+ INSTANCES_COLUMN_COR_XY_DEVICE_TWO + " real not null ,"
		+ INSTANCES_COLUMN_COR_YZ_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_COR_ZX_DEVICE_TWO + " real not null, "

		+ INSTANCES_COLUMN_MEAN_X_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Y_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Z_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_VAR_X_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_VAR_Y_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_VAR_Z_DEVICE_THREE + " real not null ,"
		+ INSTANCES_COLUMN_COR_XY_DEVICE_THREE + " real not null ,"
		+ INSTANCES_COLUMN_COR_YZ_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_COR_ZX_DEVICE_THREE + " real not null, "

		+ INSTANCES_COLUMN_MEAN_X_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Y_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Z_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_VAR_X_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_VAR_Y_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_VAR_Z_DEVICE_FOUR + " real not null ,"
		+ INSTANCES_COLUMN_COR_XY_DEVICE_FOUR + " real not null ,"
		+ INSTANCES_COLUMN_COR_YZ_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_COR_ZX_DEVICE_FOUR + " real not null, "

		+ INSTANCES_COLUMN_T_START + " real not null, "
		+ INSTANCES_COLUMN_T_END + " real not null, "
		+ INSTANCES_COLUMN_SEGMENT_NUMBER + " integer not null, " 
		+ INSTANCES_COLUMN_ACTIVITY + " integer not null); ",

		// readings tables
		"create table " + TABLE_READINGS_DEVICE_ONE + "("
		+ READINGS_COLUMN_ID + " integer primary key autoincrement, "
		+ READINGS_COLUMN_T + " real not null, "
		+ READINGS_COLUMN_X + " real not null, "
		+ READINGS_COLUMN_Y + " real not null, "
		+ READINGS_COLUMN_Z + " real not null); ",

		"create table " + TABLE_READINGS_DEVICE_TWO + "("
		+ READINGS_COLUMN_ID + " integer primary key autoincrement, "
		+ READINGS_COLUMN_T + " real not null, "
		+ READINGS_COLUMN_X + " real not null, "
		+ READINGS_COLUMN_Y + " real not null, "
		+ READINGS_COLUMN_Z + " real not null); ",

		"create table " + TABLE_READINGS_DEVICE_THREE + "("
		+ READINGS_COLUMN_ID + " integer primary key autoincrement, "
		+ READINGS_COLUMN_T + " real not null, "
		+ READINGS_COLUMN_X + " real not null, "
		+ READINGS_COLUMN_Y + " real not null, "
		+ READINGS_COLUMN_Z + " real not null); ",

		"create table " + TABLE_READINGS_DEVICE_FOUR + "("
		+ READINGS_COLUMN_ID + " integer primary key autoincrement, "
		+ READINGS_COLUMN_T + " real not null, "
		+ READINGS_COLUMN_X + " real not null, "
		+ READINGS_COLUMN_Y + " real not null, "
		+ READINGS_COLUMN_Z + " real not null); ",

		// unlabelled instances
		"create table " + TABLE_UNLABELLED_INSTANCES + "("
		+ INSTANCES_COLUMN_ID + " integer primary key autoincrement, "
		+ INSTANCES_COLUMN_MEAN_X_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Y_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Z_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_VAR_X_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_VAR_Y_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_VAR_Z_DEVICE_ONE + " real not null ,"
		+ INSTANCES_COLUMN_COR_XY_DEVICE_ONE + " real not null ,"
		+ INSTANCES_COLUMN_COR_YZ_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_COR_ZX_DEVICE_ONE + " real not null, "
		
		+ INSTANCES_COLUMN_MEAN_X_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Y_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Z_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_VAR_X_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_VAR_Y_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_VAR_Z_DEVICE_TWO + " real not null ,"
		+ INSTANCES_COLUMN_COR_XY_DEVICE_TWO + " real not null ,"
		+ INSTANCES_COLUMN_COR_YZ_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_COR_ZX_DEVICE_TWO + " real not null, "
				
		+ INSTANCES_COLUMN_MEAN_X_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Y_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Z_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_VAR_X_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_VAR_Y_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_VAR_Z_DEVICE_THREE + " real not null ,"
		+ INSTANCES_COLUMN_COR_XY_DEVICE_THREE + " real not null ,"
		+ INSTANCES_COLUMN_COR_YZ_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_COR_ZX_DEVICE_THREE + " real not null, "
						
		+ INSTANCES_COLUMN_MEAN_X_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Y_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Z_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_VAR_X_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_VAR_Y_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_VAR_Z_DEVICE_FOUR + " real not null ,"
		+ INSTANCES_COLUMN_COR_XY_DEVICE_FOUR + " real not null ,"
		+ INSTANCES_COLUMN_COR_YZ_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_COR_ZX_DEVICE_FOUR + " real not null," 
		
		+ TIMESTAMP_COLUMN + " real not null,"
		+ SMALL_CONFIDENCE_COLUMN + " real not null,"
		+ BIG_CONFIDENCE_COLUMN + " real not null); ",
						
		// cluster instances
		"create table " + TABLE_CLUSTER_INSTANCES + "("
		+ INSTANCES_COLUMN_ID + " integer primary key autoincrement, "
		
		+ INSTANCES_COLUMN_MEAN_X_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Y_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Z_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_VAR_X_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_VAR_Y_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_VAR_Z_DEVICE_ONE + " real not null ,"
		+ INSTANCES_COLUMN_COR_XY_DEVICE_ONE + " real not null ,"
		+ INSTANCES_COLUMN_COR_YZ_DEVICE_ONE + " real not null, "
		+ INSTANCES_COLUMN_COR_ZX_DEVICE_ONE + " real not null, "
		
		+ INSTANCES_COLUMN_MEAN_X_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Y_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Z_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_VAR_X_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_VAR_Y_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_VAR_Z_DEVICE_TWO + " real not null ,"
		+ INSTANCES_COLUMN_COR_XY_DEVICE_TWO + " real not null ,"
		+ INSTANCES_COLUMN_COR_YZ_DEVICE_TWO + " real not null, "
		+ INSTANCES_COLUMN_COR_ZX_DEVICE_TWO + " real not null, "
		
		+ INSTANCES_COLUMN_MEAN_X_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Y_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Z_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_VAR_X_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_VAR_Y_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_VAR_Z_DEVICE_THREE + " real not null ,"
		+ INSTANCES_COLUMN_COR_XY_DEVICE_THREE + " real not null ,"
		+ INSTANCES_COLUMN_COR_YZ_DEVICE_THREE + " real not null, "
		+ INSTANCES_COLUMN_COR_ZX_DEVICE_THREE + " real not null, "
		
		+ INSTANCES_COLUMN_MEAN_X_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Y_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_MEAN_Z_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_VAR_X_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_VAR_Y_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_VAR_Z_DEVICE_FOUR + " real not null ,"
		+ INSTANCES_COLUMN_COR_XY_DEVICE_FOUR + " real not null ,"
		+ INSTANCES_COLUMN_COR_YZ_DEVICE_FOUR + " real not null, "
		+ INSTANCES_COLUMN_COR_ZX_DEVICE_FOUR + " real not null, "		
		
		+ INSTANCES_COLUMN_ACTIVITY + " integer not null); "
	};

	public ARSQLiteHelper(Context context) {
		super(context, DATABASE_NAME, null, DATABASE_VERSION);
	}

	@Override
	public void onCreate(SQLiteDatabase db) {
		for (String strCreate : DATABASE_CREATE) {
			Log.i("DB", strCreate);
			db.execSQL(strCreate);
		}
	}

	@Override
	public void onUpgrade(SQLiteDatabase db, int oldVer, int newVer) {
		Log.i("DB", "Upgrading database");
		db.execSQL("DROP TABLE IF EXISTS " + TABLE_INSTANCES);
		
		db.execSQL("DROP TABLE IF EXISTS " + TABLE_READINGS_DEVICE_ONE);
		db.execSQL("DROP TABLE IF EXISTS " + TABLE_READINGS_DEVICE_TWO);
		db.execSQL("DROP TABLE IF EXISTS " + TABLE_READINGS_DEVICE_THREE);
		db.execSQL("DROP TABLE IF EXISTS " + TABLE_READINGS_DEVICE_FOUR);
		
		db.execSQL("DROP TABLE IF EXISTS " + TABLE_UNLABELLED_INSTANCES);
		db.execSQL("DROP TABLE IF EXISTS " + TABLE_CLUSTER_INSTANCES);
		
		onCreate(db);
	}

}

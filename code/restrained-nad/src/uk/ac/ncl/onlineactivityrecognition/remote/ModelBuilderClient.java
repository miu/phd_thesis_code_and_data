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
 
package uk.ac.ncl.onlineactivityrecognition.remote;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.SerializableEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;

import android.util.Log;

import weka.classifiers.Classifier;
import weka.core.Instances;

public class ModelBuilderClient {
	private int port;
	private String hostname;
	private String uri;
	private int timeout;
	
	private Instances dataset;

	public ModelBuilderClient(String hostname, int port, int msTimeout,
			Instances dataset) {
		this.port = port;
		this.hostname = hostname;
		uri = "http://" + hostname + ":" + port;
		timeout = msTimeout;
		this.dataset = dataset;
	}

	public Classifier getModel() throws IOException, 
	ClassNotFoundException {
		HttpClient httpClient = new DefaultHttpClient();
		HttpPost httpPost = new HttpPost(uri);
		httpPost.setEntity(new SerializableEntity(dataset, false));

		HttpParams httpParams = new BasicHttpParams();
		HttpConnectionParams.setConnectionTimeout(httpParams, timeout);
		httpPost.setParams(httpParams);

		Log.v("----------", "sending POST to " + uri);
		HttpResponse httpResponse = httpClient.execute(httpPost);
		Log.v("----------", "executed POST");
		
		HttpEntity httpEntity = httpResponse.getEntity();
		InputStream is = httpEntity.getContent();
		ObjectInputStream ois = new ObjectInputStream(is);

		Classifier model = (Classifier) ois.readObject();
		Log.v("----------", "Object read");
		
		return model;
	}
}

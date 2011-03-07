// =================================================================
// Copyright (C) 2010-2011 DFKI GmbH Talking Robots
// Miroslav Janicek (miroslav.janicek@dfki.de)
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public License
// as published by the Free Software Foundation; either version 2.1 of
// the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
// 02111-1307, USA.
// =================================================================

package de.dfki.lt.tr.infer.abducer.util;

import de.dfki.lt.tr.infer.abducer.engine.AbductionEnginePrx;
import de.dfki.lt.tr.infer.abducer.engine.AbductionEngineServerPrx;
import de.dfki.lt.tr.infer.abducer.engine.AbductionEngineServerPrxHelper;

/**
 * A wrapper for manipulating the abducer server.
 *
 * @author Miroslav Janicek
 * @version 2.1.0
 */
public class AbductionEngineConnection {

	private Ice.Communicator ic;
	private AbductionEnginePrx prx = null;
	private AbductionEngineServerPrx srvPrx = null;
	private String name = null;

	public AbductionEngineConnection() {
		ic = Ice.Util.initialize();
	}

	/**
	 * Connect to the engine server.
	 *
	 * @param serverName server Ice name
	 * @param serverEndpoint server Ice endpoint
	 */
	public void connectToServer(String serverName, String serverEndpoint) {
		try {
			ic = Ice.Util.initialize();
			Ice.ObjectPrx base = ic.stringToProxy(serverName + ":" + serverEndpoint);
			srvPrx = AbductionEngineServerPrxHelper.checkedCast(base);
			if (srvPrx == null) {
				throw new Error("Unable to create proxy");
			}
		}
		catch (Ice.LocalException e) {
			e.printStackTrace();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Bind to an engine.
	 *
	 * @param engineName name of the engine
	 */
	public void bindToEngine(String engineName) {
		if (srvPrx != null) {
			prx = srvPrx.getEngineProxy(engineName);
			name = engineName;
		}
		else {
			throw new Error("lost connection to the server");
		}
	}

	/**
	 * Return the Ice communicator of the connection to the engine server.
	 *
	 * @return the communicator
	 */
	public Ice.Communicator getCommunicator() {
		return ic;
	}

	/**
	 * Return the engine proxy.
	 * 
	 * @return the proxy, or <tt>null</tt> if not bound to an engine
	 */
	public AbductionEnginePrx getEngineProxy() {
		return prx;
	}

	/**
	 * Return the engine name.
	 *
	 * @return engine name, <tt>null</tt> if not bound to an engine
	 */
	public String getEngineName() {
		return name;
	}

}

// =================================================================
// Copyright (C) 2010 DFKI GmbH Talking Robots
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

package de.dfki.lt.tr.infer.weigabd;

import de.dfki.lt.tr.infer.weigabd.slice.AbductionEnginePrx;
import de.dfki.lt.tr.infer.weigabd.slice.AbductionEngineServerPrx;
import de.dfki.lt.tr.infer.weigabd.slice.AbductionEngineServerPrxHelper;

/**
 * A wrapper for manipulating the abducer server.
 *
 * @author Miroslav Janicek
 */
public class AbductionEngineConnection {

    protected Ice.Communicator ic;
    protected AbductionEnginePrx prx;
	protected AbductionEngineServerPrx srvPrx = null;
	protected String name = "[UNBOUND]";

    public AbductionEngineConnection() {
        ic = Ice.Util.initialize();
    }

	/**
	 * Connect to the server.
	 *
	 * @param name server Ice name
	 * @param endpoint server Ice endpoint
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
	 * Return the Ice communicator.
	 *
	 * @return
	 */
    public Ice.Communicator getCommunicator() {
        return ic;
    }

	/**
	 * Return the abducer proxy.
	 * 
	 * @return
	 */
    public AbductionEnginePrx getProxy() {
        return prx;
    }

	/**
	 * Return the engine name.
	 *
	 * @return
	 */
	public String getEngineName() {
		return name;
	}

}
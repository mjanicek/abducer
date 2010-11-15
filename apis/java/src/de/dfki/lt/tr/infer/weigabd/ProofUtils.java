// =================================================================
// Copyright (C) 2009-2010 DFKI GmbH Talking Robots
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

import de.dfki.lt.tr.infer.weigabd.slice.Atom;
import de.dfki.lt.tr.infer.weigabd.slice.AssertedQuery;
import de.dfki.lt.tr.infer.weigabd.slice.AssumedQuery;
import de.dfki.lt.tr.infer.weigabd.slice.FunctionTerm;
import de.dfki.lt.tr.infer.weigabd.slice.MarkedQuery;
import de.dfki.lt.tr.infer.weigabd.slice.ModalisedAtom;
import de.dfki.lt.tr.infer.weigabd.slice.Modality;
import de.dfki.lt.tr.infer.weigabd.slice.NullAssumabilityFunction;
import de.dfki.lt.tr.infer.weigabd.slice.Term;
import de.dfki.lt.tr.infer.weigabd.slice.UnsolvedQuery;
import java.util.ArrayList;

public class ProofUtils {

	/**
	 * Given a sequence of queries, return all queries marked as asserted.
	 * 
	 * @param qs the sequence of queries (proof)
	 * @return all elements of qs that are of type AssertedQuery
	 */
	public static AssertedQuery[] filterAsserted(MarkedQuery[] qs) {
		ArrayList<AssertedQuery> list = new ArrayList<AssertedQuery>();
		for (int i = 0; i < qs.length; i++) {
			if (qs[i] instanceof AssertedQuery) {
				list.add((AssertedQuery) qs[i]);
			}
		}
		return list.toArray(new AssertedQuery[0]);
	}
	
	/**
	 * Given a sequence of queries, return all queries that are marked as assumed.
	 * 
	 * @param qs the sequence of queries (proof)
	 * @return all elements of qs that are of type AssumedQuery
	 */
	public static AssumedQuery[] filterAssumed(MarkedQuery[] qs) {
		ArrayList<AssumedQuery> list = new ArrayList<AssumedQuery>();
		for (int i = 0; i < qs.length; i++) {
			if (qs[i] instanceof AssumedQuery) {
				list.add((AssumedQuery) qs[i]);
			}
		}
		return list.toArray(new AssumedQuery[0]);		
	}

	/**
	 * Convert a proof to a sequence of modalised atoms, thereby stripping
	 * the proof of its marking.
	 * @param qs the sequence of queries (proof)
	 * @return sequence of the corresponding modalised atoms
	 */
	public static ModalisedAtom[] stripMarking(MarkedQuery[] qs) {
		ArrayList<ModalisedAtom> list = new ArrayList<ModalisedAtom>();
		for (int i = 0; i < qs.length; i++) {
			list.add(qs[i].atom);
		}
		return list.toArray(new ModalisedAtom[0]);
	}

	/**
	 * Filter a sequence of modalised formulas by a modality prefix.
	 * Formulas thus prefixed will be included in the output with the
	 * prefix removed.
	 *
	 * TODO: evaluated on class equality rather than class *content* equality
	 *
	 * @param mas sequence of modalised atoms
	 * @param m modality prefix
	 * @return sequence of modalised formulas from mas with m removed
	 */
	public static ModalisedAtom[] filterStripByModalityPrefix(ModalisedAtom[] mas, Modality[] m) {
		ArrayList<ModalisedAtom> list = new ArrayList<ModalisedAtom>();
		for (int i = 0; i < mas.length; i++) {
			if (mas[i].m.length >= m.length) {
				boolean good = true;
				for (int j = 0; j < m.length; j++) {
					good = good && mas[i].m[j] == m[j];
				}
				if (good) {
					ArrayList<Modality> ms = new ArrayList<Modality>();
					for (int k = m.length; k < mas[i].m.length; k++) {
						ms.add(mas[i].m[k]);
					}
					ModalisedAtom ma = TermAtomFactory.modalisedAtom(
							ms.toArray(new Modality[0]),
							(Atom) mas[i].a.clone());
					list.add(ma);
				}
			}
		}
		return list.toArray(new ModalisedAtom[0]);
	}

	/**
	 * Create a new unsolved proof.
	 *
	 * @param goal the goal formula
	 * @return the proof
	 */
	public static UnsolvedQuery[] newUnsolvedProof(ModalisedAtom goal) {
		UnsolvedQuery q = new UnsolvedQuery();
		q.atom = goal;
		NullAssumabilityFunction af = new NullAssumabilityFunction();
		q.f = af;

		return new UnsolvedQuery[] {q};
	}

	/**
	 * Convert a term to string.
	 *
	 * @param t the term
	 * @return functor of t, or null if not applicable
	 */
	public static String termToString(Term t) {
		if (t instanceof FunctionTerm) {
			return ((FunctionTerm)t).functor;
		}
		else {
			return null;
		}
	}

}

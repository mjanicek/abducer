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

import de.dfki.lt.tr.infer.weigabd.slice.AssertedQuery;
import de.dfki.lt.tr.infer.weigabd.slice.AssumedQuery;
import de.dfki.lt.tr.infer.weigabd.slice.FunctionTerm;
import de.dfki.lt.tr.infer.weigabd.slice.MarkedQuery;
import de.dfki.lt.tr.infer.weigabd.slice.ModalisedAtom;
import de.dfki.lt.tr.infer.weigabd.slice.Modality;
import de.dfki.lt.tr.infer.weigabd.slice.Atom;
import de.dfki.lt.tr.infer.weigabd.slice.ProvedQuery;
import de.dfki.lt.tr.infer.weigabd.slice.Term;
import de.dfki.lt.tr.infer.weigabd.slice.UnsolvedQuery;
import de.dfki.lt.tr.infer.weigabd.slice.VariableTerm;
import java.util.Iterator;
import java.util.List;

/**
 * Utilities for converting elements of the language to strings.
 *
 * @author Miroslav Janicek
 */
public abstract class MercuryUtils {

	/** Escape the given string if it starts with an uppercase
	 *  letter or contains a '-'.
	 *
	 * @param s the string
	 * @return escaped string if necessary
	 */
	public static String termStringEscape(String s) {
		if (s.equals("") || s.contains("-") || s.contains(":") || !Character.isLowerCase(s.charAt(0))) {
			return "'" + s + "'";
		}
		else {
			return s;
		}
	}

	/** Convert a term to a string. If the term is a variable,
	 *  it is given a "V_" prefix to assure that it starts with
	 *  an uppercase letter.
	 *
	 * @param t the term
	 * @return corresponding string
	 */
	public static String termToString(Term t) {
		String s = "";

		if (t instanceof VariableTerm) {
		    VariableTerm v = (VariableTerm) t;
		    s = "V_" + v.name;
		    return s;
		}
		else if (t instanceof FunctionTerm) {
		    FunctionTerm f = (FunctionTerm) t;
		    s = termStringEscape(f.functor);
		    if (!f.args.isEmpty()) {
				s += "(";
				Iterator<Term> iter = f.args.iterator();
				while (iter.hasNext()) {
					s += termToString(iter.next());
					s += (iter.hasNext() ? "" : ", ");
				}
				s += ")";
		    }
		    return s;
		}
		else {
		    return null;
		}
	}

	/** Convert an atom to a string.
	 *
	 * @param a the atom
	 * @return corresponding string
	 */
	public static String atomToString(Atom a) {
		String s = termStringEscape(a.predSym);
		if (!a.args.isEmpty()) {
			s += "(";
			Iterator<Term> iter = a.args.iterator();
			while (iter.hasNext()) {
				s += termToString(iter.next());
				s += (iter.hasNext() ? "" : ", ");
			}
			s += ")";
		}
		return s;
	}

	/** Convert a modalised atom to a string.
	 *
	 * @param ma the modalised atom
	 * @return corresponding string
	 */
	public static String modalisedAtomToString(ModalisedAtom ma) {
		return modalitySeqToString(ma.m) + atomToString(ma.a);
	}

	/** Convert a sequence of modalities to a string. If the sequence
	 *  is non-empty, then each modality is delimited by a ':'.
	 *
	 * @param m the sequence
	 * @return corresponding string
	 */
	public static String modalitySeqToString(List<Modality> m) {
		String s = "";
		for (Modality mod : m) {
			s += modalityToString(mod) + ":";
		}
		return s;
	}

	/** Convert a modality to a string.
	 *
	 * @param m the modality
	 * @return corresponding string
	 */
    public static String modalityToString(Modality m) {
		switch (m) {
			case Truth:
				return "i";
			case Belief:
				return "bel";
			case Intention:
				return "int";
			case Attention:
				return "att";
			case Event:
				return "event";
			case Understanding:
				return "understand";
			case Generation:
				return "generate";
		}
		return "unknown";
	}

	/**
	 * Return the string representation of a proof.
	 *
	 * @param proof
	 * @return the string representation of proof
	 */
	public static String proofToString(MarkedQuery[] proof) {
		String s = "";
		for (int i = 0; i < proof.length; i++) {
			s += "  [" + markedQueryToMarkingString(proof[i]) + "]\t";
			s += MercuryUtils.modalisedAtomToString(proof[i].atom);
			if (i < proof.length-1) { s += ",\n"; }
		}
		return s;
	}

	/**
	 * Return the string representation of a marked query marking.
	 *
	 * @param q marked query
	 * @return the marking as a string
	 */
	public static String markedQueryToMarkingString(MarkedQuery q) {
		if (q instanceof UnsolvedQuery) {
			return "unsolved";
		}
		if (q instanceof ProvedQuery) {
			return "proved";
		}
		if (q instanceof AssumedQuery) {
			return "assumed";
		}
		if (q instanceof AssertedQuery) {
			return "asserted";
		}
		else {
			return "?";
		}
	}
}

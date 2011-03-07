// =================================================================
// Copyright (C) 2009-2011 DFKI GmbH Talking Robots
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

import de.dfki.lt.tr.infer.abducer.lang.FunctionTerm;
import de.dfki.lt.tr.infer.abducer.lang.ModalisedAtom;
import de.dfki.lt.tr.infer.abducer.lang.Modality;
import de.dfki.lt.tr.infer.abducer.lang.Atom;
import de.dfki.lt.tr.infer.abducer.lang.Term;
import de.dfki.lt.tr.infer.abducer.lang.VariableTerm;
import de.dfki.lt.tr.infer.abducer.proof.AssertedQuery;
import de.dfki.lt.tr.infer.abducer.proof.AssumedQuery;
import de.dfki.lt.tr.infer.abducer.proof.MarkedQuery;
import de.dfki.lt.tr.infer.abducer.proof.ProvedQuery;
import de.dfki.lt.tr.infer.abducer.proof.UnsolvedQuery;
import java.util.Iterator;
import java.util.List;

/**
 * Utilities for pretty-printing the language.
 *
 * Note that the output is not necessarily in a format compatible with
 * the loading routines of the abducer engine. In other words, if you
 * store the output in a file and try to load it using
 * {@link de.dfki.lt.tr.infer.abducer.engine.AbductionEnginePrx#loadFile(java.lang.String) AbductionEnginePrx.loadFile(...)},
 * you are likely to get a syntax error.
 *
 * @author Miroslav Janicek
 * @version 2.1.0
 */
public abstract class PrettyPrint {

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
			s += modalisedAtomToString(proof[i].atom);
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

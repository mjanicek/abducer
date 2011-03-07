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

import de.dfki.lt.tr.infer.abducer.lang.Atom;
import de.dfki.lt.tr.infer.abducer.lang.FunctionTerm;
import de.dfki.lt.tr.infer.abducer.lang.ModalisedAtom;
import de.dfki.lt.tr.infer.abducer.lang.Modality;
import de.dfki.lt.tr.infer.abducer.lang.Term;
import de.dfki.lt.tr.infer.abducer.lang.VariableTerm;
import java.util.Arrays;
import java.util.List;

/**
 * Term and atom creator helpers.
 *
 * @author Miroslav Janicek
 * @version 2.1.0
 */
public abstract class TermAtomFactory {

	/**
	 * Return a new atom.
	 * 
	 * @param predSym predicate symbol
	 * @param args arguments (terms)
	 * @return Predicate the predicate
	 */
	public static Atom atom(String predSym, Term[] args) {
		return new Atom(predSym, Arrays.asList(args));
	}

	/**
	 * Return a new atom.
	 *
	 * @param predSym predicate symbol
	 * @param args arguments (terms)
	 * @return Predicate the predicate
	 */
	public static Atom atom(String predSym, List<Term> args) {
		return new Atom(predSym, args);
	}
	
	/**
	 * Return an atom with two arguments.
	 * 
	 * @param predSym predicate symbol
	 * @param arg1 first argument
	 * @param arg2 second argument
	 * @return the predicate
	 */
	public static Atom twoPlaceAtom(String predSym, String arg1, String arg2) {
		return atom(predSym, new Term[] { term(arg1), term(arg2)});
	}
	
	/**
	 * Return a function term.
	 * 
	 * @param functor term functor
	 * @param args arguments (terms)
	 * @return FunctionTerm the term
	 */
	public static FunctionTerm term(String functor, Term[] args) {
		return new FunctionTerm(functor, Arrays.asList(args));
	}

	/**
	 * Return a function term.
	 *
	 * @param functor term functor
	 * @param args arguments (terms)
	 * @return FunctionTerm the term
	 */
	public static FunctionTerm term(String functor, List<Term> args) {
		return new FunctionTerm(functor, args);
	}

	/**
	 * Return a function term with no arguments.
	 * 
	 * @param functor term functor
	 * @return FunctionTerm the term
	 */
	public static FunctionTerm term(String functor) {
		return term(functor, new Term[0]);
	}
	
	/**
	 * Return a named variable.
	 * 
	 * @param name variable name
	 * @return VariableTerm the term
	 */
	public static VariableTerm var(String name) {
		return new VariableTerm(name);
	}

	/**
	 * Return a new modalised atom.
	 *
	 * @param ms list of modalities, m1...mn
	 * @param a the atomic formula to be modalised, p(t1...tn)
	 * @return ModalisedAtom the modalised atom, "m1...mn:p(t1...tn)"
	 */
	public static ModalisedAtom modalisedAtom(Modality[] ms, Atom a) {
		return new ModalisedAtom(Arrays.asList(ms), a);
	}

	/**
	 * Return a new modalised atom.
	 *
	 * @param ms list of modalities, m1...mn
	 * @param a the atomic formula to be modalised, p(t1...tn)
	 * @return ModalisedAtom the modalised atom, "m1...mn:p(t1...tn)"
	 */
	public static ModalisedAtom modalisedAtom(List<Modality> ms, Atom a) {
		return new ModalisedAtom(ms, a);
	}

}

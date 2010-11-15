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

import de.dfki.lt.tr.infer.weigabd.slice.FunctionTerm;
import de.dfki.lt.tr.infer.weigabd.slice.Atom;
import de.dfki.lt.tr.infer.weigabd.slice.ModalisedAtom;
import de.dfki.lt.tr.infer.weigabd.slice.Modality;
import de.dfki.lt.tr.infer.weigabd.slice.Term;
import de.dfki.lt.tr.infer.weigabd.slice.VariableTerm;
import java.util.List;

public abstract class TermAtomFactory {

	/**
	 * Return a new predicate.
	 * 
	 * @param predSym predicate symbol
	 * @param args arguments (terms)
	 * @return Predicate the predicate
	 */
	public static Atom atom(String predSym, Term[] args) {
		Atom a = new Atom();
		a.predSym = predSym;
		a.args = args;
		return a;
	}

	/**
	 * Return a new predicate.
	 *
	 * @param predSym predicate symbol
	 * @param args arguments (terms)
	 * @return Predicate the predicate
	 */
	public static Atom atom(String predSym, List<Term> args) {
		Atom a = new Atom();
		a.predSym = predSym;
		a.args = args.toArray(new Term[0]);
		return a;
	}
	
	/**
	 * Return a two-place predicate.
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
		FunctionTerm f = new FunctionTerm();
		f.functor = functor;
		f.args = args;
		return f;
	}

	/**
	 * Return a function term.
	 *
	 * @param functor term functor
	 * @param args arguments (terms)
	 * @return FunctionTerm the term
	 */
	public static FunctionTerm term(String functor, List<Term> args) {
		FunctionTerm f = new FunctionTerm();
		f.functor = functor;
		f.args = args.toArray(new Term[0]);
		return f;
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
		VariableTerm v = new VariableTerm();
		v.name = name;
		return v;
	}

	/**
	 * Return a new modalised atom.
	 *
	 * @param ms list of modalities, m1...mn
	 * @param a the atomic formula to be modalised, p(t1...tn)
	 * @return ModalisedAtom the modalised atom, "m1...mn:p(t1...tn)"
	 */
	public static ModalisedAtom modalisedAtom(Modality[] ms, Atom a) {
		ModalisedAtom ma = new ModalisedAtom();
		ma.m = ms;
		ma.a = a;
		return ma;
	}

	/**
	 * Return a new modalised atom.
	 *
	 * @param ms list of modalities, m1...mn
	 * @param a the atomic formula to be modalised, p(t1...tn)
	 * @return ModalisedAtom the modalised atom, "m1...mn:p(t1...tn)"
	 */
	public static ModalisedAtom modalisedAtom(List<Modality> ms, Atom a) {
		ModalisedAtom ma = new ModalisedAtom();
		ma.m = ms.toArray(new Modality[0]);
		ma.a = a;
		return ma;
	}

}
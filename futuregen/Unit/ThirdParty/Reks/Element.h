/**
 *  Element.h
 *
 */

// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////

#ifndef CKINTERP_ELEMENT_H
#define CKINTERP_ELEMENT_H
#pragma warning(disable : 4786)

#include <string>
#include <vector>
#include <iostream>

using namespace std;
/**
 *   A class for elements.  
 */
class REKS_element {
public:

    REKS_element() : m_name("X"), m_atomic_weight(0.0), m_valid(0), m_is_on_chart(false){}
	//Empty construction, Chemical X, not exist

    REKS_element(const string& name) :m_name(name),m_atomic_weight(0.0), m_valid(0), m_is_on_chart(false){}

	~REKS_element() {}

    string m_name;                 //  Element name
    REAL m_atomic_weight;        //  Atomic weight
    int m_valid;                     //  flag returned by validation routines
    bool m_is_on_chart;             //  true if it is on periodic chart
	REAL m_valence;

    //If two elements has the same name
    bool operator==(const REKS_element& e) const 
	{
        return (m_name == e.m_name);
    }

    bool operator!=(const REKS_element& e) const 
	{
        return !(m_name == e.m_name);
    }

    friend std::ostream& operator<<(std::ostream& s, const REKS_element& e) 
	{
        s << e.m_name;
        if (!e.m_is_on_chart) s << "/" << e.m_atomic_weight << "/";
        return s;
    }
};

/// a list (vector) of Elements pointers
typedef vector<REKS_element*>  REKS_element_list;

#endif


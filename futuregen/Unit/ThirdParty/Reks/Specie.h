/**
 *  Specie.h
 *
 */

// Copyright 2002  Reaction Engineering International


#ifndef CKINTERP_SPECIE_H
#define CKINTERP_SPECIE_H

#include <string>
#include <vector>

namespace Nckinterp {

/**
 *   A class for elements.  
 */
class CSpecie {
public:

    CSpecie() : m_name("X"){}
	//Empty construction, Chemical X, not exist

    CSpecie(const string& name) :m_name(name){}

	~CSpecie() {}

    string m_name;                 //  Specie name
    REAL m_atomic_weight;        //  Atomic weight
    int m_valid;                     //  flag returned by validation routines
    bool m_is_on_chart;             //  true if it is on periodic chart
    
    bool operator==(const CSpecie& e) const {
        return (m_name == e.m_name);
    }

    bool operator!=(const CSpecie& e) const {
        return !(m_name == e.m_name);
    }

    friend ostream& operator<<(ostream& s, const CSpecie& e) {
        s << e.m_name;
        if (!e.m_is_on_chart) s << "/" << e.m_atomic_weight << "/";
        return s;
    }
};

/// a list (vector) of Species
typedef vector<CSpecie>  CSpecieList;

}



#endif


// ElementReader.h: interface for the ElementReader class.
//
// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////////////////////


#ifndef ELEMENTREADER_H
#define ELEMENTREADER_H
#pragma warning(disable : 4786)

#include "Element.h"
#include "REKS_Thrm_Info.h"

class REKS_Thrm_Info;

class REKS_element_reader  
{
//this is a class to parse the elements part of the mechanism input file
public:
	REKS_element_reader();
	virtual ~REKS_element_reader();

	//element can only be 1 or 2 characters long
	bool element_length_check(); 

	//check if the element is on the element chart, which is defined in GlobalConst.h
	bool is_on_chart(string elem); 
	
	//check if the element is already in the list, which means it's already been read in
	bool already_in(string elem);

	//Get the weight from the element chart
	REAL weight_in_chart(string elem); 

	//line parser, parse one line, and fill the vector the elems
	bool fill_elems(vector<string> elems, int& pos, int line_num);

	//parse the elements part of the mechanism file
	void parse(vector<string> inp_files);

	//debugging function to print out the element list
	void dump();	
	
	REKS_Thrm_Info * reks_thrm_info;
};


#endif


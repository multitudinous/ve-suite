// ListTable.cpp: implementation of the ListTable class.
//
//////////////////////////////////////////////////////////////////////

#include "VE_Conductor/Framework/ListTable.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

ListTable::ListTable(wxWindow *parent,
               const wxWindowID id,
               const wxPoint& pos,
               const wxSize& size)
			   : wxListCtrl(parent, id, pos, size, wxLC_REPORT | wxSUNKEN_BORDER  ),
			    m_attr(*wxBLUE, *wxLIGHT_GREY, wxNullFont)
{

}

ListTable::~ListTable()
{

}

void ListTable::SetTitle(std::vector<wxString> titles)
{
	int i;
	int len;

	ClearAll();
	len = titles.size();
	for (i=0; i<len; i++)
		InsertColumn(i, titles[i]); // , wxLIST_FORMAT_LEFT, 140);
	num_of_row = 0;
    
}

void ListTable::AddRow(std::vector<wxString> vals)
{
	if (vals.size()>0)
		InsertItem(num_of_row, vals[0]);//, 0);
 
	for (unsigned int i=1; i<vals.size(); i++)
		SetItem(num_of_row, i, vals[i]);

	 num_of_row++;
}

void ListTable::DelRow(int i)
{
	DeleteItem(i);
}

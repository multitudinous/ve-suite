#include "TexTable.h"
#include <iostream>

TexTable::TexTable(wxWindow* parent,wxWindowID id, const wxPoint& pos, const wxSize& size)
  : wxTextCtrl(parent, id, "x", pos, size, wxTE_MULTILINE|wxTE_READONLY|wxHSCROLL|wxTE_DONTWRAP)
{
  SetNumofCols(2);
#ifndef WIN32
  ChooseFixedFont(12);
#else
  ChooseFixedFont(8);
#endif
}

void TexTable::SetNumofCols(int num)
{
  int i;
  num_cols = num;
  cols_width.resize(num);
  
  for (i=0; i<num; i++)
    cols_width[i]=17; //default width
}

void TexTable::SetColWidth(int Col_id, int width)
{
  if (Col_id<0 || Col_id>num_cols-1)
    return;
  cols_width[Col_id] = width;

}

wxString TexTable::padding(wxString str, int col_id)
{
  int len;
  
  if (col_id<0 || col_id>num_cols-1)
    return str;

  len = str.Len();
  if (len>cols_width[col_id])
    return str.Mid(0, cols_width[col_id]);
    
  if (len<cols_width[col_id])
    return str.Pad((cols_width[col_id]-len), ' ');

  return str;
    
}

bool TexTable::ChooseFixedFont(int size)
{
  wxFontEnumerator fontEnumerator;
  wxArrayString * faces;
#ifndef WIN32
  fontEnumerator.EnumerateFacenames(wxFONTENCODING_KOI8, TRUE);
#else
  fontEnumerator.EnumerateFacenames(wxFONTENCODING_CP1250, TRUE);
#endif
  wxString facename;
  // choose the first
  faces = fontEnumerator.GetFacenames();
  
  facename = faces->Item(0);
  //  std::cout<<facename<<" : A Font"<<std::endl;
  if ( !facename.IsEmpty() )
    {
      wxFont font(size, wxTELETYPE, wxNORMAL, wxNORMAL, FALSE, facename);
      //  std::cout<<"A valid font"<<std::endl;
      DoChangeFont(font);
      return TRUE;
    }
  
  return FALSE;
}

void TexTable::DoChangeFont(const wxFont &font)
{
  SetFont(font);
}

void TexTable::AddRow(const std::vector<wxString>& vals)
{
  int i;
   
  SetBackgroundColour(*wxWHITE);
  for (i=0; i<num_cols; i++)
    {
      if (i<vals.size())
	AppendText(padding(vals[i], i));
      else
	AppendText(padding(" ", i));
    }
  AppendText("\n");
}

void TexTable::AddSeperator(char pad)
{
  int i;
  wxString str;
  SetBackgroundColour(*wxWHITE);
  for (i=0; i<num_cols; i++)
    {
      str="";
      AppendText(str.Pad(cols_width[i], pad));
    }
  AppendText("\n");
}

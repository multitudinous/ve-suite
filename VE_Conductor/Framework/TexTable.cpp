/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/TexTable.h"
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
    cols_width[i]=30; //default width
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
      if (i<(int)vals.size())
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

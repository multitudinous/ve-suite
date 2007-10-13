/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/conductor/TexTable.h>
#include <iostream>

#include <wx/window.h>
using namespace ves::conductor;


TexTable::TexTable(wxWindow* parent,wxWindowID id, const wxPoint& pos, const wxSize& size)
  : wxGrid(parent, id, pos, size)
//wxTextCtrl(parent, id, "x", pos, size, wxTE_MULTILINE|wxTE_READONLY|wxHSCROLL|wxTE_DONTWRAP)
{
  CreateGrid (0,2);
  SetColLabelValue (0, _("Description") );
  SetColLabelValue (1, _("Value") );
  SetRowMinimalAcceptableHeight (9);
  SetColMinimalAcceptableWidth (27);
  SetColMinimalWidth (0, 27);
  SetColMinimalWidth (1, 27);
  AutoSizeColumns (FALSE);
  AutoSizeRows (FALSE);
  EnableEditing (FALSE);
  //SetNumofCols(2);
#ifndef WIN32
  ChooseFixedFont(12);
#else
  ChooseFixedFont(8);
#endif
}

void TexTable::SetNumofCols(int num)
{
   if ( GetNumberRows() )
      DeleteRows( 0, GetNumberRows() );
   
   if ( GetNumberCols() )
      DeleteCols( 0, GetNumberCols() );

   if ( num > 0 ) 
      AppendCols( num );

   SetRowMinimalAcceptableHeight( 9 );
   SetColMinimalAcceptableWidth( 27 );

   for ( int i=0; i < GetNumberCols(); i++ )
   {
      SetColMinimalWidth( i, 27 );
   }
/*
  int i;
  num_cols = num;
  cols_width.resize(num);
  
  for (i=0; i<num; i++)
    cols_width[i]=30; //default width*/
}

void TexTable::SetColTitles(const std::vector<wxString>& titles)
{
   for ( size_t i=0; 
         (i < static_cast<size_t>(titles.size()) ) && ( i < static_cast<size_t>(GetNumberCols()) ); ++i)
   {
      SetColLabelValue( i, titles[i] );
   }
}

void TexTable::SetColAlignments(const std::vector<int>& alignments)
{
   m_align = alignments;
   for ( size_t i=0; 
         (i <  static_cast<size_t>(alignments.size()) ) && ( i < static_cast<size_t>(GetNumberCols()) ); ++i )
   {
      for ( size_t j = 0; j <  static_cast<size_t>(GetNumberRows()); ++j)
      {
         SetCellAlignment( j, i, alignments[i], wxALIGN_CENTER);
      }
   }
}

void TexTable::SetColWidth(int Col_id, int width)
{
  /*if (Col_id<0 || Col_id>num_cols-1)
    return;
  cols_width[Col_id] = width;*/
   if ( ( width < 0 ) || 
         ( Col_id < 0 ) || 
         ( Col_id >= GetNumberCols() ) ) 
   {
      return;
   }
   SetColMinimalWidth( Col_id, width );
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
   faces = &fontEnumerator.GetFacenames();

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
   SetDefaultCellFont(font);
   //SetFont(font);
}

void TexTable::AddRow(const std::vector<wxString>& vals)
{
   /*  
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
   */

   AppendRows( 1 );
   for ( int i=0; i<GetNumberCols(); ++i )
   {
      wxString temp;
      if ( i< (int)vals.size() )
      {
         temp = vals.at( i );
      }
      else
      {
         temp = _("");
      }

      SetCellValue( GetNumberRows()-1, i, temp);
      if ( i >= (int)m_align.size() ) 
         continue;
      SetCellAlignment(GetNumberRows()-1, i, m_align[i], wxALIGN_CENTER);
   }
   SetRowMinimalHeight(GetNumberRows()-1, 9);
   //AutoSizeColumns( FALSE );
   //AutoSizeRows( FALSE );
}

void TexTable::AddSeperator(char pad)
{
/*  int i;
  wxString str;
  SetBackgroundColour(*wxWHITE);
  for (i=0; i<num_cols; i++)
    {
      str="";
      AppendText(str.Pad(cols_width[i], pad));
    }
  AppendText("\n");*/
 
   std::vector<wxString> vals;

   for ( int i=0; i < GetNumberCols(); ++i )
   {
      wxString s = _("");
      s.Pad( 10, pad );
      vals.push_back( s );
   }
   AddRow( vals );
}

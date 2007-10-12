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
#ifndef TEXT_TABLE_H
#define TEXT_TABLE_H
/*!\file TexTable.h
TexTable API
*/
/*!\class TexTable
* 
*/
#include <vector>
#include <wx/grid.h>
#include <wx/fontenum.h>

class wxWindow;

#include <ves/VEConfig.h>

namespace ves
{
namespace conductor
{
class VE_GUIPLUGINS_EXPORTS TexTable : public wxGrid
{
public:
   TexTable(wxWindow* parent, wxWindowID id = -1, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize);
   virtual ~TexTable(){;}
   void Clear( void ){ ClearGrid(); SetNumofCols(2); }
   void SetColWidth( int Col_id, int width );
   void SetNumofCols( int num );
   void SetColTitles( const std::vector<wxString>& titles );
   void SetColAlignments( const std::vector<int>& alignments );

   void AddRow( const std::vector<wxString>& vals );
   void AddSeperator( char pad='=' );
   void DoChangeFont( const wxFont &font );
   bool ChooseFixedFont( int size );
   wxString padding( wxString str, int col_id );

private:
   int num_cols;
   std::vector< int > m_align;
   std::vector< int > cols_width;
};
}
}
#endif

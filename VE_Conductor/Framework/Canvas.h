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
* Date modified: $Date: 2007-10-05 20:50:26 -0500 (Fri, 05 Oct 2007) $
* Version:       $Rev: 9227 $
* Author:        $Author: mccdo $
* Id:            $Id: Network.cxx 9227 2007-10-06 01:50:26Z mccdo $
* -----------------------------------------------------------------
*
*************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CANVAS_H
#define CANVAS_H
/*!\file Canvas.h
Canvas API
*/
/*!\class Canvas
* 
*/

#include <wx/event.h>
#include <wx/scrolwin.h>
#include <wx/textdlg.h>
#include <wx/menu.h>
#include <wx/dcclient.h>

class AppFrame;
class Network;

#include <vector>
#include <map>

class Canvas : public wxScrolledWindow
{
public:
    ///Default canvas
    Canvas(){;}
    ///Constructor
    Canvas(wxWindow* parent, int id );
    ///Destructor
    virtual ~Canvas();
    ///On Paint controls the draw loop for the canvas
    void OnPaint( wxPaintEvent &event );
    ///Get the active network being rendered
    Network * GetActiveNetwork();
    ///Set active network id from hierarchy tree
    void SetActiveNetwork(std::string id);
    ///Populate canvas with network string
    void PopulateNetworks( std::string xmlNetwork );
    ///User scale
    /// first = x scale
    /// second = y scale
    std::pair< double, double > userScale;
    ///Net design canvas
    void New( bool promptClearXplorer );
    ///Get the correct size for sub dialogs
    wxRect GetAppropriateSubDialogSize();
    ///Delete and remove the network event handlers
    void CleanUpNetworks();

protected:

private:
    std::map < std::string, Network* > networks;
    std::string activeId;
    std::string previousId;
    void DrawNetwork(wxDC &dc, std::string id);
    DECLARE_EVENT_TABLE() // no semicolon needed
};

#endif

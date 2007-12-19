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
    Canvas()
    {
        ;
    }
    ///Constructor
    Canvas( wxWindow* parent, int id );
    ///Destructor
    virtual ~Canvas();
    ///On Paint controls the draw loop for the canvas
    void OnPaint( wxPaintEvent &event );
    ///Get the active network being rendered
    Network * GetActiveNetwork();
    std::string GetActiveNetworkID( );
    ///Set active network id from hierarchy tree
    void SetActiveNetwork( std::string id );
    ///Populate canvas with network string
    void PopulateNetworks( std::string xmlNetwork, bool clearXplorer = true );
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
    ///Creates a default network for the user to work on a clean canvas
    void CreateDefaultNetwork();
    ///Update all the network
    void Update();
    ///Set the window that holds the object tree to determine the size
    ///of the child windows
    void SetTreeViewWindow( wxWindow* treeView );
    void OnDelMod( wxCommandEvent& event );
    void OnChangeIcon( wxCommandEvent& event );

protected:

private:
    std::map < std::string, Network* > networks;
    std::string activeId;
    std::string previousId;
    void DrawNetwork( wxDC &dc, std::string id );
    wxWindow* m_treeView;
    wxWindow* parent;
    DECLARE_EVENT_TABLE() // no semicolon needed
};

#endif

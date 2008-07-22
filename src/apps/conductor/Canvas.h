/*************** <auto-copyright.rb BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
*************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef VES_APP_CANVAS_H
#define VES_APP_CANVAS_H
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

#include <vector>
#include <map>

class AppFrame;
class Network;

class Canvas : public wxScrolledWindow
{
public:
    enum CANVAS_ENUMS
    {
        UPDATE_NETWORK_DATA = 3700
    };
    
    ///Default canvas
    Canvas();
    ///Constructor
    Canvas( wxWindow* parent, int id );
    ///Destructor
    virtual ~Canvas();
    ///On Paint controls the draw loop for the canvas
    ///\param event wxWidgets event
    void OnPaint( wxPaintEvent& event );
    ///Get the active network being rendered
    ///\return The active network as selected by the user
    Network* GetActiveNetwork();
    ///Get the id of the active network, this is a guid
    ///\return The id guid as a string
    std::string GetActiveNetworkID();
    ///Set active network id from hierarchy tree
    ///\param id The guid of the network to be activated
    void SetActiveNetwork( std::string id );
    ///Populate canvas with network string
    ///Note: Must call Canvas::New( bool ) before calling this function
    void PopulateNetworks( std::string xmlNetwork, bool clearXplorer = true );
    ///Net design canvas
    void New( bool promptClearXplorer );
    ///Get the correct size for sub dialogs
    ///\return The wxRect for this scrolled window
    wxRect GetAppropriateSubDialogSize();
    ///Creates a default network for the user to work on a clean canvas
    void CreateDefaultNetwork();
    ///Update all the network
    void Update();
    ///Set the window that holds the object tree to determine the size
    ///of the child windows
    void SetTreeViewWindow( wxWindow* treeView );
    ///Event handler to delete modules
    void OnDelMod( wxCommandEvent& event );
    ///Event handler to delete networks
    void OnDelNetwork( wxUpdateUIEvent& event );
    ///Delete and remove the network event handlers
    void CleanUpNetworks();
    ///Set the User scale
    void SetUserScale( double x, double y );
    void SetMainFrame(wxWindow *window);

private:
    ///Map of the networks for this system
    std::map< std::string, Network* > networks;
    ///active guid
    std::string activeId;
    ///previous guid
    std::string previousId;
    ///Draw function
    void DrawNetwork( wxDC &dc, std::string id );
    //used for adding a system to a plugin
    void CreateNewSystem( wxCommandEvent& event );
    ///treeview widget
    wxWindow* m_treeView;
    ///parent window
    wxWindow* parent;
    ///canvas is cleaned up
    wxUpdateUIEvent cleanEvent;
    std::pair< double, double > userScale;
    wxWindow* mainFrame;

    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }
    void OnZoom( wxKeyEvent &event );
    
    DECLARE_EVENT_TABLE() // no semicolon needed
};

#endif /// VES_APP_CANVAS_H

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
#ifndef AVAIL_MODULES_H
#define AVAIL_MODULES_H
/*!\file Avail_Modules.h
Avail_Modules API
*/
/*!\class Avail_Modules
*
*/
#include <vector>
#include <wx/image.h>
#include <wx/imaglist.h>
#include <wx/treectrl.h>

//Forward declaration of classes
namespace ves
{
    namespace conductor
    {
        class UIPluginBase;
        class Canvas;
    }
}
class AppFrame;
class PluginLoader;

class Avail_Modules : public wxTreeCtrl
{
public:

    ///Default constructor
    Avail_Modules()
    {
        ;
    }
    
    ///Normal constructor
    Avail_Modules( wxWindow *parent, const wxWindowID id, const wxPoint& pos,
        const wxSize& size, long style );
    
    ///Destructor
    virtual ~Avail_Modules();

    enum
    {
        TreeCtrlIcon_File,
        TreeCtrlIcon_FileSelected,
        TreeCtrlIcon_Folder,
        TreeCtrlIcon_FolderSelected,
        TreeCtrlIcon_FolderOpened,
        Module_Desc,
        Module_Help,
        Module_Add,
        TREE_CTRL = 1000
    };
    
    ///right click event
    void OnItemRightClick( wxTreeEvent& event );

    ///single click event
    void OnSelChanged( wxTreeEvent& event );

    ///right click menu
    void ShowMenu( wxTreeItemId id, const wxPoint &pt );

    ///Create image list of size
    ///\param size Size of images
    void CreateImageList( int size = 16 );
    
    void AddModule( ves::conductor::UIPluginBase* plugin, wxClassInfo* clsi );
    
    ///module description
    void ShowDesc( wxCommandEvent& event );
    
    ///help menu
    void ShowHelp( wxCommandEvent& event );
    
    ///Creates new module and adds it to the canvas and hierarchy tree
    void Instantiate( wxTreeEvent& event );
    
    ///Set the frame to work with
    ///\param frm frame to work with    
    void SetFrame( AppFrame *frm )
    {
        frame = frm;
    };

    ///Set the canvas to work with
    ///\param can canvas to work with    
    void SetCanvas( ves::conductor::Canvas *can )
    {
        canvas = can;
    };
    
    ///Get the new plugin tree after reseting it
    void ResetPluginTree( void );

protected:
    ///Load all the modules from the dlls
    bool LoadModules(); 

    ///parse out the location and name for an item
    void getLeveledName( wxString name, std::vector<wxString> & lnames );

    ///utility unicode conversion method
    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >
            ( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }    

    ///the size to rescale of the images in the list
    int m_imageSize;

    ///used to load custom plug-ins
    PluginLoader* pl_loader;

    ///tree ids
    wxTreeItemId rootId;
    wxTreeItemId selection;

    ///a pointer to the main frame
    AppFrame * frame;

    ///a pointer to the main canvas
    ves::conductor::Canvas* canvas;

    ///the list of images
    wxImageList *images;

    DECLARE_EVENT_TABLE();


};

///class for storing information regarding modules
class ReiTreeItemData : public wxTreeItemData
{
public:
    ves::conductor::UIPluginBase* plugin;
    wxClassInfo* pl_clsi;

    ReiTreeItemData( ves::conductor::UIPluginBase* pl,  wxClassInfo* clsi )
    {
        plugin = pl;
        pl_clsi = clsi;
    };
};

#endif

/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 * Date modified: $Date: 2009-06-28 23:47:14 -0700 (Sun, 28 Jun 2009) $
 * Version:       $Rev: 12939 $
 * Author:        $Author: akubach $
 * Id:            $Id: AppFrame.cxx 12939 2009-06-29 06:47:14Z akubach $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

///////////////////////////////////////////////////////////////////////////////
//
//  Dialog for Minerva properties.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MINERVA_DIALOG_H_
#define _MINERVA_DIALOG_H_

#include <ves/VEConfig.h>

#include <ves/open/xml/CommandPtr.h>

#include <wx/dialog.h>

#include <vector>
#include <string>

class wxButton;
class wxListBox;
class wxStdDialogButtonSizer;

class VE_GUIPLUGINS_EXPORTS MinervaDialog : public wxDialog
{
public:
    typedef wxDialog BaseClass;

    MinervaDialog ( wxWindow *parent, wxWindowID id );
    virtual ~MinervaDialog();

    void AddDefaultLayers();

    void AddElevationLayerWMS ( wxCommandEvent& event );
    void AddElevationLayerFileSystem ( wxCommandEvent& event );
    void RemoveElevationLayer ( wxCommandEvent& event );

    void AddRasterLayerWMS ( wxCommandEvent& event );
    void AddRasterLayerFileSystem ( wxCommandEvent& event );
    void RemoveRasterLayer ( wxCommandEvent& event );

    void InitalizeFromCommands ( ves::open::xml::CommandPtr elevationGroupCommand, ves::open::xml::CommandPtr rasterGroupCommand );

private:

    typedef std::vector<std::string> LayerIds;

    static void _addLayerFileSystem ( 
      const std::string& commandName, 
      const std::string& filename,
      wxListBox *layersList, 
      LayerIds &guids,
      ves::open::xml::CommandPtr groupCommand );
    static void _addLayer ( 
      const std::string& commandName, 
      const std::string& server, 
      const std::string& layers, 
      const std::string& styles, 
      const std::string& format,
      wxListBox *layersList, 
      LayerIds &guids,
      ves::open::xml::CommandPtr groupCommand );
    static void _removeLayer ( 
      const std::string& commandName, 
      wxListBox *layersList, 
      LayerIds &guids,
      ves::open::xml::CommandPtr groupCommand );
    static void _initializeFromCommand (
      ves::open::xml::CommandPtr groupCommand,
      wxListBox *layersList, 
      LayerIds &guids );

    wxListBox* _elevationLayersList;
    wxButton* _addElevationLayerButton;
    wxButton* _removeElevationLayerButton;
    wxListBox* _rasterLayersList;
    wxButton* _addRasterLayerButton;
    wxButton* _removeRasterLayerButton;
    wxStdDialogButtonSizer* _sdbSizer1;
    wxButton* _sdbSizer1OK;
    wxButton* _sdbSizer1Cancel;

    LayerIds _elevationLayers;
    LayerIds _rasterLayers;

    ves::open::xml::CommandPtr _rasterGroupCommand;
    ves::open::xml::CommandPtr _elevationGroupCommand;
    
    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }
    
    DECLARE_EVENT_TABLE();
};

#endif // _MINERVA_DIALOG_H_

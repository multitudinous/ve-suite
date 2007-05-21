/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#ifndef SOUNDS_PANE_H
#define SOUNDS_PANE_H

/*!\file SoundsPane.h
*/

/*!\class SoundsPane
* 
*/

// --- wxWidgets Includes --- //
#include <wx/dialog.h>

class wxCheckListBox;
class wxButton;

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

namespace VE_XML
{
class Command;

namespace VE_Model
{
    class Model;
}
}

class SoundsPane : public wxDialog
{
public:
    SoundsPane( VE_XML::VE_Model::Model* model );
    virtual ~SoundsPane(){;}

    enum SOUNDS_TAB_IDS
    {
        SOUND_CBOX,
        SOUND_LOAD_BUTTON
    };

    void SendCommandsToXplorer();

    ///Set the active VE_XML::VE_Model::Model
    ///\param model The active model;
    void SetActiveModel(VE_XML::VE_Model::Model* model);

protected:
    void _buildPage();

    //the controls
    wxCheckListBox* _soundCBox;
    wxButton* _loadButton;

    unsigned int _numSounds;///<The number of sounds
    VE_XML::VE_Model::Model* _activeModel;///<The current model;
    wxArrayString _loadedSounds;///<The sounds loaded already;

    ///Update the current sound information from a given VE_Model
    void _updateSoundsInformationFromModel();

    ///Check if we've already loaded this sound file
    ///\param filename The file to check
    bool _ensureSounds( wxString filename );

    ///Clear the loaded sounds
    void _clearLoadedSounds();

    ///Load the sound files in xplorer
    ///\param soundFileName The name of the sound file to load
    void _loadSoundsInXplorer( wxString soundFileName );

    //event handlers
    void _onSounds( wxCommandEvent& event );  
    void _onLoadAndUpdate( wxCommandEvent& event );  

    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

    DECLARE_EVENT_TABLE()
};
#endif //SOUNDS_PANE_H

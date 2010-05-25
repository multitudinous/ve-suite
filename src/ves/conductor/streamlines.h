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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef STREAM_LINES_H
#define STREAM_LINES_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/open/xml/DataValuePairPtr.h>

// --- wxWidgets Includes --- //
#include <wx/dialog.h>

class wxRadioBox;
class wxSlider;
class wxButton;
class wxComboBox;

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

#define ID_DIALOG 10000
#define SYMBOL_STREAMLINES_STYLE wxCAPTION | wxRESIZE_BORDER | wxSYSTEM_MENU | wxCLOSE_BOX
#define SYMBOL_STREAMLINES_TITLE _T( "Streamlines" )
#define SYMBOL_STREAMLINES_IDNAME 10000
#define SYMBOL_STREAMLINES_SIZE wxSize( 400, 300 )
#define SYMBOL_STREAMLINES_POSITION wxDefaultPosition

namespace ves
{
namespace conductor
{

namespace util
{
class WPDialog;
}

/*!\file streamlines.h
 *streamlines API
 */

/*!\class Streamlines
 *
 */
class VE_GUIPLUGINS_EXPORTS Streamlines : public wxDialog
{
public:
    Streamlines();
    Streamlines( wxWindow* parent,
                 wxWindowID id = SYMBOL_STREAMLINES_IDNAME,
                 const wxString& caption = SYMBOL_STREAMLINES_TITLE,
                 const wxPoint& pos = SYMBOL_STREAMLINES_POSITION,
                 const wxSize& size = SYMBOL_STREAMLINES_SIZE,
                 long style = SYMBOL_STREAMLINES_STYLE );

    virtual ~Streamlines();

    void SendCommandsToXplorer();

    ///Creation
    bool Create( wxWindow* parent,
                 wxWindowID id = SYMBOL_STREAMLINES_IDNAME,
                 const wxString& caption = SYMBOL_STREAMLINES_TITLE,
                 const wxPoint& pos = SYMBOL_STREAMLINES_POSITION,
                 const wxSize& size = SYMBOL_STREAMLINES_SIZE,
                 long style = SYMBOL_STREAMLINES_STYLE );

    ///Creates the controls and sizers
    void CreateControls();

    ///Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    ///Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );

    ///Should we show tooltips?
    static bool ShowToolTips();
    ///Set the active dataset name
    void SetActiveDataSetName( std::string name );

    ///Set the available datasets so that they can be selected for surface mapping
    void SetAvailableDatasets( wxArrayString tempNames );

    wxButton* itemButton14;

protected:
    void _onCursorSelect( wxCommandEvent& event );
    void _onDirection( wxCommandEvent& event );
    void _onIntegrateDir( wxCommandEvent& event );
    void _onPointsSlider( wxScrollEvent& event );
    void _onAdvanced( wxCommandEvent& event );
    void _onSizeSlider( wxScrollEvent& event );
    void _onCompute( wxCommandEvent& event );
    void SetSeedPoints( wxCommandEvent& event );
    void OnClose( wxCommandEvent& event );

    ///Turn GPU tools off and on
    void OnGPUCheckTools( wxCommandEvent& event );
    ///Turn SURF source off and on
    void OnSURFCheckTools( wxCommandEvent& event );
    ///Update the streamlines information
    void _updateStreamlineInformation();
    ///Update the advanced settings
    void _updateAdvancedSettings();

    wxRadioBox* _cursorRBox;
    wxRadioBox* _directionRBox;
    wxRadioBox* _integrationRBox;
    wxSlider* _sizeSlider;
    wxSlider* _nPointsSlider;
    wxButton* itemButton13;
    wxCheckBox* m_gpuToolsChkBox;
    wxCheckBox* m_surfToolsChkBox;
    ///The array containing all of the datasets used for the surface tools
    wxArrayString   m_availableDatasets;
    ///The box listing the available datasets.
    wxComboBox*     m_datasetSelection;
    
    std::vector< ves::open::xml::DataValuePairPtr > _advancedSettings;///<The advanced settings.
    std::vector< ves::open::xml::DataValuePairPtr > _streamlineInformation;///<The streamline setting data
    std::vector< ves::open::xml::DataValuePairPtr > seedPointInformation;///<The streamline setting data

    double _streamSize;///<The size of the streamlines.
    unsigned int _nPointsPerPlane;///<The number of seed points.
    std::string _streamlineDirection;///<Store the value of the direction.
    std::string _cursorType;///<The contour type.
    std::string _integrationDirection;///<Single or Multiple planes.
    ///Active dataset name
    std::string dataSetName;

    double _lastIntegrationStepSize;///<Integration step size from advanced settings.
    double _lastPropagationSize;///<Propagation size from advanced settings.
    double _lastLineDiameter;///<Line diameter from advanced settings.
    double _lastSphereArrowParticleSize;///<Sphere arrow particles from advanced settings.
    bool _lastSeedPtFlag;///<Seed pt flat from advanced settings.
    bool _lastStreamArrow;///<Stream arrow from advanced settings.
    bool m_animatedParticles;
    bool m_streamRibbons;
    
    int cId, cIso_value, cMin, cMax, cSc;
    std::vector< long > commandInputs;

    ves::conductor::util::WPDialog* seedPointDialog;
    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

    DECLARE_EVENT_TABLE()
};
}
}
#endif // end STREAM_LINES_H

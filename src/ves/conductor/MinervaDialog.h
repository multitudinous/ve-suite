
///////////////////////////////////////////////////////////////////////////////
//
//  Dialog for Minerva properties.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _MINERVA_DIALOG_H_
#define _MINERVA_DIALOG_H_

#include <wx/dialog.h>

#include <vector>
#include <string>

#include <ves/VEConfig.h>

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

  void AddElevationLayer ( wxCommandEvent& event );
  void RemoveElevationLayer ( wxCommandEvent& event );

  void AddRasterLayer ( wxCommandEvent& event );
  void RemoveRasterLayer ( wxCommandEvent& event );

private:

  typedef std::vector<std::string> LayerIds;

  static void _addLayer ( 
    const std::string& commandName, 
    const std::string& server, 
    const std::string& layers, 
    const std::string& styles, 
    const std::string& format,
    wxListBox *layersList, 
    LayerIds &guids );
  static void _removeLayer ( const std::string& commandName, wxListBox *layersList, LayerIds &guids );

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

  DECLARE_EVENT_TABLE();
};

#endif // _MINERVA_DIALOG_H_

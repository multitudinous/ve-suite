#ifndef GEOMETRY_DIALOG
#define GEOMETRY_DIALOG

#ifdef WIN32
#include <winsock2.h>
#endif
#include "wx/wx.h"
#include "wx/dialog.h"
#include "VE_Conductor/Framework/GeometryDataManager.h"

enum GEOMETRY_CONFIG_IDS
{
   GEOMETRY_CONFIG_RBOX,
   GEOMETRY_CONFIG_CBOX,
   GEOMETRY_CONFIG_UPDATE_BUTTON,
   GEOMETRY_CONFIG_OPACITY_SLIDER,
   GEOMETRY_CONFIG_LOD_SLIDER
};

class GeometryDialog : public wxDialog
{
   public: 
      //GeometryDialog( wxWindow *parent, wxWindowID id );
      
      GeometryDialog(   wxWindow* parent,
                        wxWindowID id =-1,
                        const wxString &title =" Geometry Dialog ",
                        const wxPoint& pos = wxDefaultPosition,
                        const wxSize& size = wxDefaultSize,
                        long  style = wxDEFAULT_DIALOG_STYLE
                     );  
      
      ~GeometryDialog(){}

      virtual bool TransferDataFromWindow();
      virtual bool TransferDataToWindow();

   protected:
     // void _buildPage();

     //the controls
      wxRadioBox* _geometryRBox;
      wxCheckListBox* _geometryCBox;
      wxButton* _updateButton;
      wxWindow* _parent;
      wxSlider* geomOpacitySlider;
      wxSlider* geomLODSlider;

      //event handlers
      void ChangeOpacity( wxScrollEvent& event );
      void _onGeometry( wxScrollEvent& event );
      void _onUpdate(wxCommandEvent& event);

       protected:
   //UI widgets variables
      wxTextCtrl* t_geomname;
      wxTextCtrl* t_geomfilename;
      wxTextCtrl* t_transparencytoggle;
      wxTextCtrl* t_colorflag;
      wxTextCtrl* t_scale0, *t_scale1, *t_scale2;
      wxTextCtrl* t_tran0, *t_tran1, *t_tran2;
      wxTextCtrl* t_rot0, *t_rot1, *t_rot2;
      wxTextCtrl* t_color0, *t_color1, *t_color2;
      wxTextCtrl* t_LOD;

      DECLARE_EVENT_TABLE()

};

#endif


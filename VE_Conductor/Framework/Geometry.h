#ifndef GEOMETRY_H
#define GEOMETRY_H
#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>

#include "interface.h"
#include <map>
#include <vector>

enum GEOMETRY_CONFIG_IDS
{
   GEOMETRY_CONFIG_RBOX,
   GEOMETRY_CONFIG_CBOX,
   GEOMETRY_CONFIG_UPDATE_BUTTON,
   GEOMETRY_CONFIG_OPACITY_SLIDER,
   GEOMETRY_CONFIG_LOD_SLIDER
};

class Geometry : public wxDialog
{
   public:
      Geometry( wxWindow *parent, wxWindowID id );
      ~Geometry( void ){ ; }
      
      // This is the load function of the module, unpack the input 
      // string and fill up the UI according to this
      void UnPack( Interface* );
      // Does the opposite of unpack
      Interface* Pack( void );

   protected:
      void _buildPage();

      // data holder 
      Interface mod_pack;
      int id;

      // Geometry data
      std::map<std::string, long *>                      _int;
      std::map<std::string, double *>                    _double;
      std::map<std::string, std::vector<long> * >        _int1D;
      std::map<std::string, std::vector<double> * >      _double1D;
      std::map<std::string, std::vector<std::string> * > _string1D;

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

      DECLARE_EVENT_TABLE()
};
#endif //GEOMETRY_H


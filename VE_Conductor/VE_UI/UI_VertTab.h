#ifndef _VE_UI_VERTEXDATA_TAB_H_
#define _VE_UI_VERTEXDATA_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>

enum VERTEXDATA_TAB_IDS{
   PARTICLE_OPTIONS_RBOX,   
   DISPLAY_PARTICLE_BUTTON,
   SPHERE_POINT_SIZE_SLIDER,
   SPHERE_SIZE_SLIDER,
   PARTICLE_OPTION_RBOX,
   DISPLAY_PARTICLES_BUTTON
};


class UI_VertTab : public wxPanel{
public:
   UI_VertTab(wxNotebook* tControl);
protected:
   void _buildPage();

   wxNotebook* _parent;
   //the event controls
 
   wxRadioBox* _particleOptionRBox;
   wxButton* _displayParticlesButton;
   wxSlider* _spherePointSizeSlider;
   

   //event handling callbacks
   void _onParticleOption(wxCommandEvent& event);
   void _onDisplayParticle(wxCommandEvent& event);
   void _onSpherePointSizeSlider(wxScrollEvent& event);   
   void ConstructCommandId( void );

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_VERTEXDATA_TAB_H_

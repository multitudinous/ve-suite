#ifndef FERMENTOR_UI_H
#define FERMENTOR_UI_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIPluginBase.h>

// --- wxWidgets Includes --- //
#include <wx/image.h>

// --- C/C++ Libraries --- //
#include <string>

class FermentorUI : public ves::conductor::UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( Fermentor )

public:
    FermentorUI();
    ~FermentorUI();

    virtual double GetVersion();
    //Return the version number of the module

    virtual void DrawIcon(wxDC* dc);
    //This call return a window to be displayed on the framework

    //To Get around the Memory allocation problem of windows dll
    //Add the calls for the size. So the main program can preallocate memory for it

    virtual int GetNumPoly();

    //virtual void GetPoly(POLY &polygon); 
    //Return the outline polygon

    virtual ves::conductor::UIDialog* UI( wxWindow* parent );
    //This returns the UI dialog of the module

    virtual wxString GetConductorName();

    wxString GetName();
    //This returns the name of the module

    virtual wxString GetDesc();
    //This returns the description of the module, This should be a short description


    //To Get around the Memory allocation problem of windows dll
    //Add the calls for the size. So the main program can preallocate memory for it

    virtual int GetNumIports();
    virtual void GetIPorts( POLY& ports );

    virtual int GetNumOports();
    virtual void GetOPorts( POLY& ports );

    double agitation;
    double air_conc;
    double ini_ph;
    double nitrate_conc;
    double temperature;
    double hours;

    long cycle_ID;
    long rotation_ID;
    long xray_ID;
    long loop_ID;

    double rot_speed;
    double sim_speed;
    //HERE is the GUI variable passed to the Dialog and Packed
  
protected:
    wxBitmap *my_icon;
    int icon_w, icon_h;
};

#endif //FERMENTOR_UI_H


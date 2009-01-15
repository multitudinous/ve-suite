#ifndef Int_Stove_Econ_H
#define Int_Stove_Econ_H

#include <ves/conductor/UIPluginBase.h>
#include <wx/image.h>

#include <vector>
using namespace std;

class Int_Stove_Econ : public ves::conductor::UIPluginBase
{
   DECLARE_DYNAMIC_CLASS(Int_Stove_Econ)

public:
   Int_Stove_Econ();
   virtual ~Int_Stove_Econ();

   virtual double GetVersion();
   //Return the version number of the module

   //To Get around the Memory allocation problem of windows dll
   //Add the calls for the size. So the main program can preallocate memory for it

   virtual ves::conductor::UIDialog* UI(wxWindow* parent);
   //This returns the UI dialog of the module

   virtual wxString GetName();
   //This returns the name of the module

   //To Get around the Memory allocation problem of windows dll
   //Add the calls for the size. So the main program can preallocate memory for it

   virtual int GetNumIports();
   virtual void GetIPorts(POLY& ports);

   virtual int GetNumOports();
   virtual void GetOPorts(POLY& ports);

public:
  std::vector< double > cost_array;
  long closesheets;
};

#endif


#ifndef WP_DIALOG_H
#define WP_DIALOG_H


#include <string> 
#include "VE_Installer/include/VEConfig.h"
#include "VE_Conductor/Utilities/BaseDialog.h"
#include "VE_Conductor/Utilities/DualSlider.h"
namespace VE_Conductor
{

namespace GUI_Utilities
{
class VE_CONDUCTOR_UTILS_EXPORTS WPDialog : public BaseDialog

{
public:
   //Constructor
      WPDialog(wxWindow* parent, int id, std::string title);
   
   //Destructor
   virtual ~WPDialog();
   
   
   ///Set the name of the command
   void SetCommandName(std::string name);
  
   ///Send the commands to Xplorer;
   void SendCommands();

   ///Add an instruction to send. This is for access in the callbacks.
   void AddInstruction(VE_XML::DataValuePair* newInstruct);
   
   
   void SetCommand(std::string name){;}
   
   ///Get the seed point vector
   std::vector< VE_XML::DataValuePair* > GetSeedPointDVPVector( void );
   ///Callback used to set the dvp vector
   void SetVectorDVP( void );
   ///Transfer the seed points info from the window
   virtual bool TransferDataFromWindow( void );
   
protected:
   
   class WPMinSliderCallback: public
   	VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
      
      WPMinSliderCallback(WPDialog* parent, std::string direction = "X")
      
      {
         _direction = direction;
	 _wpdlg = parent;
      }
      
      virtual ~WPMinSliderCallback()
      {
         _direction.clear();
      }
      
      virtual void SliderOperation();
      
      protected:
      std::string _direction;
      WPDialog* _wpdlg;
    };


   class WPBothMoveCallback: public
   	VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
      
      WPBothMoveCallback(WPDialog* parent, std::string direction = "X")
      
      {
         _direction = direction;
	 _wpdlg = parent;
      }
      
      virtual ~WPBothMoveCallback()
      {
         _direction.clear();
      }
      
      virtual void SliderOperation();
      
      protected:
      std::string _direction;
      WPDialog* _wpdlg;
    };



   class WPMaxSliderCallback: public
   	VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
      
      WPMaxSliderCallback(WPDialog* parent, std::string direction = "X")
      
      {
         _direction = direction;
	 _wpdlg = parent;
      }
      
      virtual ~WPMaxSliderCallback()
      {
         _direction.clear();
      }
      
      virtual void SliderOperation();
      
      protected:
      std::string _direction;
      WPDialog* _wpdlg;
    };

   //Build the DualSliders for the dialog
   void _createDualSliders();

   //Add controls for dialog
   virtual void _buildGUI();
   virtual wxSizer* _buildSpecificWidgets();
   
   //
   VE_Conductor::GUI_Utilities::DualSlider* _xBounds;
   VE_Conductor::GUI_Utilities::DualSlider* _yBounds;
   VE_Conductor::GUI_Utilities::DualSlider* _zBounds;
   std::vector< VE_XML::DataValuePair* > seedPointDVP;
};

}

}
#endif  //_WP_DIALOG_H

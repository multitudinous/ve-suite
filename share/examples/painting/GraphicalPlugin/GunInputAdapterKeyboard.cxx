#include "GunInputAdapterKeyboard.h"

// **************************************************************************
// Handle the keyboard and mouse button presses.

bool GunInputAdapterKeyboard::handle(const osgGA::GUIEventAdapter &ea, osgGA::GUIActionAdapter &aa)

{
   switch (ea.getEventType())
      {
      case osgGA::GUIEventAdapter::KEYDOWN:
         KeyPress(ea);
         break;
      case osgGA::GUIEventAdapter::KEYUP:
         KeyRelease(ea);
         break;
      }
   return false;
}

// **************************************************************************

void GunInputAdapterKeyboard::KeyPress(const osgGA::GUIEventAdapter &ea)

{
   if (_gun.valid() == false)
      return;
   switch (ea.getKey())
      {
      case ',':
         _gun->DecPitch();
         break;
      case '.':
         _gun->IncPitch();
         break;
      case '[':
         _gun->DecViscosity();
         break;
      case ']':
         _gun->IncViscosity();
         break;
      case ';':
         _gun->DecFluidPressure();
         break;
      case '\'':
         _gun->IncFluidPressure();
         break;
      case '-':
         _gun->DecOrifice();
         break;
      case '=':
      case '+':
         _gun->IncOrifice();
         break;
      case '9':
         _gun->DecFan();
         break;
      case '0':
         _gun->IncFan();
         break;
      default:
         break;
      }
}

// **************************************************************************

void GunInputAdapterKeyboard::KeyRelease(const osgGA::GUIEventAdapter &ea)

{
}

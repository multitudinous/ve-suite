#ifndef GUN_H
#define GUH_H

#include <osg/Referenced>

// Name: VirtualPaint Demo
// Gun.h
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011

// --------------------------------------------------------------------------

class Gun : public osg::Referenced
   {
   public:
      Gun() {Clear();}
      virtual ~Gun() {}

      // set all values to defaults.
      void Clear()
         {
         distance = 12.0;
         distanceMin = 0;
         distanceMax = 36;
         distanceInc = 0.25;
         pitch = 0;
         pitchMin = -90.0;
         pitchMax = 90.0;
         pitchInc = 10.0;
         viscosity = 100.0;
         viscosityMin = 10.0;
         viscosityMax = 1000.0;
         viscosityInc = 10.0;
         fluidPressure = 3000.0;
         fluidPressureMin = 100.0;
         fluidPressureMax = 10000.0;
         fluidPressureInc = 100.0;
         orifice = 0.023;
         orificeMin = 0.001;
         orificeMax = 0.1;
         orificeInc = 0.001;
         fan = 18.0;
         fanMin = 0.5;
         fanMax = 36.0;
         fanInc = 0.5;
         position._v[0] = 0;
         position._v[1] = 0;
         position._v[2] = 0;
         aim._v[0] = 0;
         aim._v[1] = 0;
         aim._v[2] = 0;
         triggerPullAmount = 0;
         _changed = true;
         }

      bool IsChanged() const {return _changed;}
      void SetChanged() {_changed = true;}
      void ClearChanged() {_changed = false;}

      void SetDistance(double newValue)
         {
         distance = newValue;
         if (distance > distanceMax)
            distance = distanceMax;
         else if (distance < distanceMin)
            distance = distanceMin;
         SetChanged();
         }
      void SetDistanceMin(double newValue) {distanceMin = newValue;}
      void SetDistanceMax(double newValue) {distanceMax = newValue;}
      void SetDistanceInc(double newValue) {distanceInc = newValue;}
      double GetDistance() const {return distance;}
      double GetDistanceMin() const {return distanceMin;}
      double GetDistanceMax() const {return distanceMax;}
      double GetDistanceInc() const {return distanceInc;}
      void IncDistance() {SetDistance(GetDistance() + GetDistanceInc());}
      void DecDistance() {SetDistance(GetDistance() - GetDistanceInc());}

      void SetPitch(double newValue)
         {
         pitch = newValue;
         if (pitch > pitchMax)
            pitch = pitchMax;
         else if (pitch < pitchMin)
            pitch = pitchMin;
         SetChanged();
         }
      void SetPitchMin(double newValue) {pitchMin = newValue;}
      void SetPitchMax(double newValue) {pitchMax = newValue;}
      void SetPitchInc(double newValue) {pitchInc = newValue;}
      double GetPitch() const {return pitch;}
      double GetPitchMin() const {return pitchMin;}
      double GetPitchMax() const {return pitchMax;}
      double GetPitchInc() const {return pitchInc;}
      void IncPitch() {SetPitch(GetPitch() + GetPitchInc());}
      void DecPitch() {SetPitch(GetPitch() - GetPitchInc());}

      void SetViscosity(double newValue)
         {
         viscosity = newValue;
         if (viscosity > viscosityMax)
            viscosity = viscosityMax;
         else if (viscosity < viscosityMin)
            viscosity = viscosityMin;
         SetChanged();
         }
      void SetViscosityMin(double newValue) {viscosityMin = newValue;}
      void SetViscosityMax(double newValue) {viscosityMax = newValue;}
      void SetViscosityInc(double newValue) {viscosityInc = newValue;}
      double GetViscosity() const {return viscosity;}
      double GetViscosityMin() const {return viscosityMin;}
      double GetViscosityMax() const {return viscosityMax;}
      double GetViscosityInc() const {return viscosityInc;}
      void IncViscosity() {SetViscosity(GetViscosity() + GetViscosityInc());}
      void DecViscosity() {SetViscosity(GetViscosity() - GetViscosityInc());}

      void SetFluidPressure(double newValue)
         {
         fluidPressure = newValue;
         if (fluidPressure > fluidPressureMax)
            fluidPressure = fluidPressureMax;
         else if (fluidPressure < fluidPressureMin)
            fluidPressure = fluidPressureMin;
         SetChanged();
         }
      void SetFluidPressureMin(double newValue) {fluidPressureMin = newValue;}
      void SetFluidPressureMax(double newValue) {fluidPressureMax = newValue;}
      void SetFluidPressureInc(double newValue) {fluidPressureInc = newValue;}
      double GetFluidPressure() const {return fluidPressure;}
      double GetFluidPressureMin() const {return fluidPressureMin;}
      double GetFluidPressureMax() const {return fluidPressureMax;}
      double GetFluidPressureInc() const {return fluidPressureInc;}
      void IncFluidPressure() {SetFluidPressure(GetFluidPressure() + GetFluidPressureInc());}
      void DecFluidPressure() {SetFluidPressure(GetFluidPressure() - GetFluidPressureInc());}

      void SetOrifice(double newValue)
         {
         orifice = newValue;
         if (orifice > orificeMax)
            orifice = orificeMax;
         else if (orifice < orificeMin)
            orifice = orificeMin;
         SetChanged();
         }
      void SetOrificeMin(double newValue) {orificeMin = newValue;}
      void SetOrificeMax(double newValue) {orificeMax = newValue;}
      void SetOrificeInc(double newValue) {orificeInc = newValue;}
      double GetOrifice() const {return orifice;}
      double GetOrificeMin() const {return orificeMin;}
      double GetOrificeMax() const {return orificeMax;}
      double GetOrificeInc() const {return orificeInc;}
      void IncOrifice() {SetOrifice(GetOrifice() + GetOrificeInc());}
      void DecOrifice() {SetOrifice(GetOrifice() - GetOrificeInc());}

      void SetFan(double newValue)
         {
         fan = newValue;
         if (fan > fanMax)
            fan = fanMax;
         else if (fan < fanMin)
            fan = fanMin;
         SetChanged();
         }
      void SetFanMin(double newValue) {fanMin = newValue;}
      void SetFanMax(double newValue) {fanMax = newValue;}
      void SetFanInc(double newValue) {fanInc = newValue;}
      double GetFan() const {return fan;}
      double GetFanMin() const {return fanMin;}
      double GetFanMax() const {return fanMax;}
      double GetFanInc() const {return fanInc;}
      void IncFan() {SetFan(GetFan() + GetFanInc());}
      void DecFan() {SetFan(GetFan() - GetFanInc());}

      void SetPosition(osg::Vec3 &pos) {position = pos;}
      osg::Vec3 GetPosition() const {return position;}

      void SetAim(osg::Vec3 &a) {aim = a;}
      osg::Vec3 GetAim() const {return aim;}

      void SetTriggerPullAmount(float tpa) {triggerPullAmount = tpa;}
      float GetTriggerPullAmount() const {return triggerPullAmount;}

   protected:
      double distance;           // in inches
      double distanceMin;
      double distanceMax;
      double distanceInc;        // increment value (+/-)
      double pitch;              // in ?? (degrees?)
      double pitchMin;
      double pitchMax;
      double pitchInc;
      double viscosity;          // in flow rate / sec.
      double viscosityMin;
      double viscosityMax;
      double viscosityInc;
      double fluidPressure;      // in pounds per square inch (PSI)
      double fluidPressureMin;
      double fluidPressureMax;
      double fluidPressureInc;
      double orifice;            // in ?? (inches? assume diameter?)
      double orificeMin;
      double orificeMax;
      double orificeInc;
      double fan;                // in inches
      double fanMin;
      double fanMax;
      double fanInc;

      osg::Vec3 position;
      osg::Vec3 aim;
      float triggerPullAmount;   // this is a percentage value, from 0.0 (off), to 1.0 (fully open / on).

   private:
      bool _changed;
   };

#endif

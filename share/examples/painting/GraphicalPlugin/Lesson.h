#ifndef LESSON_H
#define LESSON_H

// Name: VirtualPaint Demo
// Lesson.h
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011

// --------------------------------------------------------------------------

class Lesson
   {
   public:
      Lesson() {Clear();}

      void Clear()
         {
         targetThicknessMin = 5.0;
         targetThicknessMax = 50.0;
         runThickness = 12.0;
         totalPaintAccum = 0;
         _changed = true;
         }

      bool IsChanged() const {return _changed;}
      void SetChanged() {_changed = true;}
      void ClearChanged() {_changed = false;}

      void SetTargetThicknessMin(double newValue) {targetThicknessMin = newValue; _changed = true;}
      double GetTargetThicknessMin() const {return targetThicknessMin;}
      void SetTargetThicknessMax(double newValue) {targetThicknessMax = newValue; _changed = true;}
      double GetTargetThicknessMax() const {return targetThicknessMax;}
      void SetRunThickness(double newValue) {runThickness = newValue; _changed = true;}
      double GetRunThickness() const {return runThickness;}

      // note: the total paint accumulation doesn't flag the object as changed. This is because
      // currently there is no need for a HUD update when this value changes.
      void SetTotalPaintAccumulation(double newValue) {totalPaintAccum = newValue;}
      double AccumulatePaint(double paintAmount) {totalPaintAccum += paintAmount; return totalPaintAccum;}
      double GetTotalPaintAccumulation() const {return totalPaintAccum;}

   protected:
      double targetThicknessMin;       // target minimum paint thickness, in mils.
      double targetThicknessMax;       // target maximum paint thickness, in mils.
      double runThickness;             // the paint thickness at which 'run' (drip) starts to occur.
      double totalPaintAccum;          // accumulator for total paint output during the lesson.

   private:
      bool _changed;
   };

#endif

#include "VE_Conductor/Utilities/Polygon.h"

using namespace VE_Conductor::GUI_Utilities;

////////////////////////////////////////////////
Polygon::Polygon( void )
{
   ;//Do nothing since this class doesn't manage any of the pointers memory
}
////////////////////////////////////////////////
Polygon::~Polygon( void )
{
   ;//Do nothing since this class doesn't manage any of the pointers memory
}
////////////////////////////////////////////////
Polygon::Polygon( const Polygon& input )
{
   poly = input.poly;
}
////////////////////////////////////////////////
Polygon& Polygon::operator= ( const Polygon& input )
{
   if ( this != &input )
   {
      poly.clear();
      poly = input.poly;
   }
   return *this;
}
////////////////////////////////////////////////
wxPoint* Polygon::GetPoint( size_t i )
{
   try
   {
      return &(poly.at( i ));
   }
   catch (...)
   {
      poly.push_back( wxPoint() );
      return &(poly.at( i ));
   }
}
////////////////////////////////////////////////
void Polygon::SetPoint( wxPoint newPoint )
{
   poly.push_back( newPoint );
}
////////////////////////////////////////////////
std::vector< wxPoint >* Polygon::GetPolygon( void )
{
   return &(poly);
}
////////////////////////////////////////////////
size_t Polygon::GetNumberOfPoints( void )
{
   return poly.size();
}
/////////////////////////////////////////////////////////////
int Polygon::inside( wxPoint pt ) 
{
/*
   int i, count = 0, j = 0;

   POLY lt, lp, lv;

   lt.push_back(pt); lt.push_back(pt); lt[1].x = 999999;
   lp.push_back(pt); lp.push_back(pt);
   lv.push_back(pt); lv.push_back(pt);

   wxPoint p(poly.back());
   poly.insert(poly.begin(), 1, p);

   double numsides = poly.size()-1;
   for(i=1; i<=numsides; i++) 
   {
      lp[0] = poly[i];
      lp[1] = poly[i-1];
      if ( intersect(lv, lp) ) 
         return 1;
   
      lp[1] = poly[i];

      if ( !intersect(lp, lt) ) 
      {
         lp[1] = poly[j];
         if ( intersect(lp, lt) ) 
            count++;
         else
            if ( i!=j+1 && ((ccw(lt[0], lt[1], poly[j])*(ccw(lt[0], lt[1], poly[i])) < 1)))
               count++;
         
         j = i;
      }
   }
   
   if(j!=numsides && ccw(lt[0], lt[1], poly[j])*ccw(lt[0], lt[1], poly[1]) == 1)
      count--;

   return count & 1;
*/
}

//////////////////////////////////////////////////////////////////
double Polygon::nearpnt(wxPoint pt, Polygon &Near)
{
/*  int i, i2;

  double t, d, dist = 99999, numsides = poly.size();

  Near.clear();

  for(i=0; i<numsides; i++) {
    i2 = i+1;
    if(i2 == numsides) i2 = 0;

    wxRealPoint p;
    wxRealPoint v(poly[i2].x-poly[i].x, poly[i2].y-poly[i].y);
    wxRealPoint n(pt.x - poly[i].x, pt.y - poly[i].y);

    t = (n.x*v.x + n.y*v.y) / (v.x*v.x + v.y*v.y);
    if(t <= 0) {
      p.x = poly[i].x;
      p.y = poly[i].y;
    } else if(t >= 1) {
      p.x = poly[i2].x;
      p.y = poly[i2].y;
    } else {
      p.x = poly[i].x+t*v.x;
      p.y = poly[i].y+t*v.y;
    }

    d = computenorm(pt, wxPoint(((int) p.x), ((int) p.y)));
    if(d < dist) {
      Near.clear();
      dist = d;
      Near.push_back(wxPoint(((int) p.x), ((int) p.y)));
    } else if(d==dist)
      Near.push_back(wxPoint(((int) p.x), ((int) p.y)));
  }

  return dist;
*/
}

/////////////////////////////////////////////////////////////
void Polygon::TransPoly( int x, int y, Polygon &newpoly)
{
/*  newpoly.clear();  
  for ( unsigned int i=0; i<oldpoly.size(); i++)
    newpoly.push_back(wxPoint(oldpoly[i].x+x, oldpoly[i].y+y));
*/   
}
///////////////////////////////////////////////////////////////
int Polygon::intersect(Polygon l1, Polygon l2)
{
/*  int ccw11 = ccw(l1[0], l1[1], l2[0]);
  int ccw12 = ccw(l1[0], l1[1], l2[1]);
  int ccw21 = ccw(l2[0], l2[1], l1[0]);
  int ccw22 = ccw(l2[0], l2[1], l1[1]);
 
  return(((ccw11*ccw12 < 0) && (ccw21*ccw22 < 0)) ||
	 (ccw11*ccw12*ccw21*ccw22 == 0));
*/
}

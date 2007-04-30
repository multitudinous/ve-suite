/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CADEntityHelper_H
#define CADEntityHelper_H
/*!\file CADEntityHelper.h
CADEntityHelper API
*/

/*!\class VE_SceneGraph::CADEntityHelper
*
*/
#include <iostream>
#include <fstream>
#include <memory>

template<typename Elem, typename Tr = std::char_traits<Elem> >
class progress_streambuf: public std::basic_filebuf<Elem, Tr>
{
public:
	typedef std::basic_filebuf<Elem, Tr> base_type;
   
	explicit progress_streambuf(const std::string &filename):	
      base_type(),
		count_(0),
		prev_perc_(0)
   {
      if ( base_type::open(filename.c_str(), std::ios_base::in | std::ios_base::binary) )
      {
         size_ = static_cast<int>(std::streambuf::pubseekoff(0, std::ios_base::end, std::ios_base::in));
         std::streambuf::pubseekoff(0, std::ios_base::beg, std::ios_base::in);
      }
   }

protected:
   virtual typename std::basic_filebuf<Elem, Tr>::int_type uflow()
	{
      typename std::basic_filebuf<Elem, Tr>::int_type v = base_type::uflow();
      count_ += std::streambuf::egptr() - std::streambuf::gptr();
      int p = count_ * 40 / size_;
      if (p > prev_perc_)
      {
         std::cout << "*";
         prev_perc_ = p;
      }
      return v;
	}
   
private:
   int count_;
	int size_;
	int prev_perc_;
};

typedef progress_streambuf<char> progbuf;

#ifdef _OSG
#include <osg/Node>
#include <osg/ref_ptr>

namespace osg 
{ 
   class Fog; 
   class LightModel;
}
#elif _OPENSG
#endif

#include "VE_Installer/include/VEConfig.h"

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS CADEntityHelper
{
public:   
   CADEntityHelper();
   //Copy constructor
   CADEntityHelper( const CADEntityHelper& );

   virtual ~CADEntityHelper( void );

   //Equal operator
   CADEntityHelper& operator= ( const CADEntityHelper& );

   //Set the name of the CADEntityHelper
   void SetName( std::string name );

   //Toggle the display of this CADEntityHelper on/off
   //\param onOff Turn on/off rendering of this CADEntityHelper\n
   //Valid values are:\n
   //ON == display this CADEntityHelper\n
   //OFF == hide this CADEntityHelper\n
   void ToggleDisplay( std::string onOff );

   //Toggle the display of this CADEntityHelper on/off
   //\param onOff Turn on/off rendering of this CADEntityHelper\n
   void ToggleDisplay( bool onOff );

#ifdef _OSG
   virtual osg::Node* GetNode( void );
#elif _OPENSG
#endif

   void LoadFile( std::string,
                  #ifdef _OSG
                  bool isStream=false
                  #endif
                  );
protected:
#ifdef _OSG
   osg::ref_ptr<osg::Node> cadNode;
   osg::ref_ptr<osg::LightModel> lightModel;
#elif _OPENSG
#endif

   bool twosidedlighting;

};
}

#endif  //CADEntityHelper_H

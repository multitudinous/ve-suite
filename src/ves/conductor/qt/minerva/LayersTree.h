/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 * Date modified: $Date: 2009-06-28 23:47:14 -0700 (Sun, 28 Jun 2009) $
 * Version:       $Rev: 12939 $
 * Author:        $Author: akubach $
 * Id:            $Id: LayersTree.h 12939 2009-06-29 06:47:14Z akubach $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef __VES_CONDUCTOR_QT_MINERVA_LAYERS_TREE_H__
#define __VES_CONDUCTOR_QT_MINERVA_LAYERS_TREE_H__

#include <QtGui/QWidget>

namespace Minerva { namespace QtWidgets { class LayersTree; } }
namespace Minerva { namespace Core { namespace Data { class Feature; } } }

namespace ves {
namespace conductor {
namespace qt {
namespace minerva {

class LayersTree : public QWidget
{
public:

  typedef QWidget BaseClass;

  LayersTree ( QWidget *parent = 0x0 );
  virtual ~LayersTree();

  void buildTree ( Minerva::Core::Data::Feature * feature );

private:

  Minerva::QtWidgets::LayersTree *mLayersTree;

};

}
}
}
}

#endif // __VES_CONDUCTOR_QT_MINERVA_LAYERS_TREE_H__

/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef __VES_CONDUCTOR_QT_MINERVA_LAYERS_TREE_H__
#define __VES_CONDUCTOR_QT_MINERVA_LAYERS_TREE_H__

#include <QtGui/QWidget>

namespace Minerva { namespace QtWidgets { class TreeControl; } }
namespace Minerva { namespace Core { namespace Data { class Feature; } } }

namespace ves {
namespace conductor {
namespace qt {
namespace minerva {

class LayersTree : public QWidget
{
    Q_OBJECT;
public:

    typedef QWidget BaseClass;

    LayersTree ( QWidget *parent = 0x0 );
    virtual ~LayersTree();

    void BuildTree ( Minerva::Core::Data::Feature * feature );

    Minerva::Core::Data::Feature* GetCurrentFeature() const;

Q_SIGNALS:

    void addLayerRequested();

protected Q_SLOTS:

    void _onContextMenuShow ( const QPoint& pos );
    void _addLayer();

private:

    Minerva::QtWidgets::TreeControl *m_treeControl;

};

}
}
}
}

#endif // __VES_CONDUCTOR_QT_MINERVA_LAYERS_TREE_H__

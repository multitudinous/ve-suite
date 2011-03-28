/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
 * Date modified: $Date: 2011-01-03 22:17:45 -0600 (Mon, 03 Jan 2011) $
 * Version:       $Rev: 15339 $
 * Author:        $Author: mccdo $
 * Id:            $Id: LayersTree.h 15339 2011-01-04 04:17:45Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef __VES_CONDUCTOR_QT_MINERVA_ADD_LAYER_WIDGET_H__
#define __VES_CONDUCTOR_QT_MINERVA_ADD_LAYER_WIDGET_H__

#include <QtGui/QWidget>

namespace Minerva { namespace QtWidgets { template <class Layer> class AddNetworkLayerWidget; } }
namespace Minerva { namespace Core { namespace Layers { class RasterLayerWms; } } }
namespace Minerva { namespace Core { namespace Data { class Container; } } }

namespace ves {
namespace conductor {
namespace qt {
namespace minerva {

    class AddFileSystemStackedWidget;

class AddLayerWidget : public QWidget
{
    Q_OBJECT;
public:

    typedef QWidget BaseClass;
    typedef Minerva::QtWidgets::AddNetworkLayerWidget<Minerva::Core::Layers::RasterLayerWms> AddWmsLayerWidget;

    AddLayerWidget ( QWidget *parent = 0x0 );
    virtual ~AddLayerWidget();

    /// Get a list of features that the user wants to add
    void AddLayersToFeature ( Minerva::Core::Data::Container* );

Q_SIGNALS:

    void accepted();
    void rejected();

private:

    AddFileSystemStackedWidget* m_addFileSystemStackedWidget;
    AddWmsLayerWidget* m_addWmsLayerWidget;
};

}
}
}
}

#endif // __VES_CONDUCTOR_QT_MINERVA_ADD_LAYER_WIDGET_H__

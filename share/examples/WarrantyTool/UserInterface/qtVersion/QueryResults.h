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
#pragma once

#include <QtGui/QWidget>
#include <QtCore/QStringList>
#include <QtGui/QListWidgetItem>
#include <QtGui/QTreeWidgetItem>

#include <string>
#include <vector>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

namespace Ui {
    class QueryResults;
}

class QueryResults : public QWidget
{
    Q_OBJECT

public:
    explicit QueryResults(QWidget *parent = 0);
    ~QueryResults();

    /// Sets the category headers for results
    void SetHeaders( const QStringList& headers );

    /// Puts the query results into a tree widget
    void PopulateResults( std::vector< QStringList > const& results );

protected:
    void changeEvent(QEvent *e);

protected slots:
    /// "Highlight All" button click; highlight all parts in query results.
    void on_m_highlightAllButton_clicked();

    /// Selection was made in query results widget; highlight the selected part
    /// in scene.
    void on_m_queryResults_currentItemChanged(QTreeWidgetItem* current,QTreeWidgetItem* previous);

private:  
    Ui::QueryResults *ui;

    /// Stores the part number for each part in this result set
    std::vector< std::string > m_partNumbers;

    /// Signal to highlight a single part
    ves::util::StringSignal_type m_highlightPartSignal;

    /// Signal to highlight multiple parts
    switchwire::Event< void(std::vector<std::string>&) > m_highlightPartsSignal;
};

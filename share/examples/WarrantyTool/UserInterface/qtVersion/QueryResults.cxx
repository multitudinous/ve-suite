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
#include "QueryResults.h"
#include "ui_QueryResults.h"

#include <ves/conductor/qt/UITabs.h>
#include <ves/conductor/qt/NaturalSortQTreeWidgetItem.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <QtCore/QString>
#include <QtGui/QTreeWidget>
#include <QtGui/QTreeWidgetItem>

#include <boost/lexical_cast.hpp>

#include <string>
#include <vector>


QueryResults::QueryResults(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::QueryResults)
{
    ui->setupUi(this);

    switchwire::EventManager* evm =
        switchwire::EventManager::instance();

    {
        std::string signalName = "QueryResults" +
        boost::lexical_cast<std::string>( this ) + ".HighlightPart";
        evm->RegisterSignal( ( &m_highlightPartSignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }

    {
        std::string signalName = "QueryResults" +
        boost::lexical_cast<std::string>( this ) + ".WarrantyToolHighlightParts";
        evm->RegisterSignal( ( &m_highlightPartsSignal ),
                            signalName, switchwire::EventManager::unspecified_SignalType );
    }
}
////////////////////////////////////////////////////////////////////////////////
QueryResults::~QueryResults()
{
    delete ui;
}
////////////////////////////////////////////////////////////////////////////////
void QueryResults::SetHeaders( const QStringList& headers )
{
    ui->m_queryResults->setColumnCount( headers.count() );
    ui->m_queryResults->setHeaderLabels( headers );
}
////////////////////////////////////////////////////////////////////////////////
void QueryResults::PopulateResults( std::vector<QStringList> const& results )
{
    for( size_t index = 0; index < results.size(); ++index )
    {
        QTreeWidgetItem* item = new ves::conductor::NaturalSortQTreeWidgetItem(
                    ui->m_queryResults, results.at( index ) );
        item = 0;
        m_partNumbers.push_back( results.at( index ).at( 0 ).toStdString() );
    }
    ui->m_queryResults->setSortingEnabled( true );
}
////////////////////////////////////////////////////////////////////////////////
void QueryResults::changeEvent( QEvent *e )
{
    QWidget::changeEvent(e);
    switch (e->type()) {
    case QEvent::LanguageChange:
        ui->retranslateUi(this);
        break;
    default:
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void QueryResults::on_m_highlightAllButton_clicked()
{
    // Clear out previous highlight before doing new
    m_highlightPartSignal.signal( "" );
    if( !m_partNumbers.empty() )
    {
        m_highlightPartsSignal.signal( m_partNumbers );
    }
}
////////////////////////////////////////////////////////////////////////////////
void QueryResults::on_m_queryResults_currentItemChanged(QTreeWidgetItem* current,
                                                        QTreeWidgetItem* )
{
    std::string partNumber = current->text( current->columnCount() - 1 ).
            toStdString();
    m_highlightPartSignal.signal( partNumber );
}
////////////////////////////////////////////////////////////////////////////////
void QueryResults::SetSelectedPartNumber( const std::string& partNumber )
{
    QList< QTreeWidgetItem* > partNums = ui->m_queryResults->findItems( QString::fromStdString( partNumber ), Qt::MatchFixedString, 0 );
    if( !partNums.empty() )
    {
        QTreeWidgetItem* item = partNums.at( 0 );
        ui->m_queryResults->setCurrentItem( item );
        //ui->m_queryResults->setCurrentIndex( );
    }
}
////////////////////////////////////////////////////////////////////////////////

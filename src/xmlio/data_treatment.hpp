#ifndef __XMLIO_CDataTreatment__
#define __XMLIO_CDataTreatment__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "node_type.hpp"
#include "mpi_manager.hpp"
#include "data_output.hpp"
#include "duration.hpp"
#include "client.hpp"
#include "xios_manager.hpp"

namespace xmlioserver
{
   namespace data
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CDataTreatment
      {
         public :

            /// Construteurs ///
            CDataTreatment
               (boost::shared_ptr<CContext> _ctxt =
                  CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId()));

            CDataTreatment(const CDataTreatment & data_treatment);       // Not implemented yet.
            CDataTreatment(const CDataTreatment * const data_treatment); // Not implemented yet.

            /// Accesseurs ///
            const boost::shared_ptr<CContext> getCurrentContext(void) const;
            const std::vector<boost::shared_ptr<CFile> > & getEnabledFiles  (void) const;

            /// Ecriture et mise à jour du calendrier ///
            void write_data(const StdString & fieldId,
                            const StdString & fileId,
                            const std::deque<ARRAY(double, 1)> & data);

            template <StdSize N>
               void write_data(const StdString & fieldId, const ARRAY(double, N) & data);

            template <StdSize N>
               void write_data(const StdString & fieldId, const ARRAY(float, N) & data);

            void update_calendar(int step);
            void set_timestep(const date::CDuration & duration);

            /// Création des sorties ///
            template <class T> void createDataOutput(void);

            /// Destructeur ///
            ~CDataTreatment(void);

         private :

            /// Traitements ///
            void doTreatment(void);

            void findAllEnabledFields(void);
            void solveAllGridRef(void);
            void solveAllOperation(void);
            void solveAllInheritance(void) const;
            void findEnabledFiles(void);

            /// Propriétés privées ///
            boost::shared_ptr<CContext> currentContext;
            std::vector<boost::shared_ptr<CFile> > enabledFiles;

      }; // CDataTreatment

      //----------------------------------------------------------------

      template <class T>
         void CDataTreatment::createDataOutput(void)
      {
         std::vector<boost::shared_ptr<CFile> >::const_iterator
            it = this->enabledFiles.begin(), end = this->enabledFiles.end();
         
         for (; it != end; it++)
         {
            boost::shared_ptr<CFile> file = *it;
            StdString filename = (!file->name.isEmpty())
                               ? file->name.getValue() : file->getId();
            StdOStringStream oss;
            oss << "wk/data/" << filename << "_node" << comm::CMPIManager::GetCommRank() << ".nc";
            boost::shared_ptr<io::CDataOutput> dout(new T(oss.str(), false));
            file->initializeDataOutput(dout);
         }
      }

      template <StdSize N>
         void CDataTreatment::write_data
            (const StdString & fieldId, const ARRAY(float, N) & data)
      {
         std::vector<boost::multi_array<double, N>::size_type shape() > shape;
         const size_type *	shapearr = data->shape();

         shape.assign(shapearr, shapearr + N);
         ARRAY_CREATE(datad, double, N, shape);
         datad->assign(data->begin(), data->end());

         this->write_data(fieldId, datad);
      }

      template <StdSize N>
         void CDataTreatment::write_data
            (const StdString & fieldId, const ARRAY(double, N) & data)
      {
         const date::CDate & currDate =
                this->currentContext->getCalendar()->getCurrentDate();
         const std::vector<boost::shared_ptr<CField> > & refField=
               CObjectFactory::GetObject<CField>(fieldId)->getAllReference();
         std::vector<boost::shared_ptr<CField> >::const_iterator
               it = refField.begin(), end = refField.end();

         for (; it != end; it++)
         {
            boost::shared_ptr<CField> field = *it;
            boost::shared_ptr<CFile>  file  = field->getRelFile();
            if (field->updateData(currDate, data))
            {
               if (CXIOSManager::GetStatus() == CXIOSManager::LOC_CLIENT)
               { 
                   boost::shared_ptr<comm::CClient> client = comm::CClient::GetClient();
                   client.sendData(fieldId, file->getId(), field->getData());
               }
               else
               {
                  file->getDataOutput()->writeFieldData(field);
               }
            }
            return;
         }

      }

   } // namespace data

} // namespace xmlioserver

#endif // __XMLIO_CDataTreatment__

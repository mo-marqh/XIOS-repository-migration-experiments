#include "data_treatment.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"
#include "object_factory_impl.hpp"


namespace xmlioserver
{
   namespace data
   {
      /// ////////////////////// Définitions ////////////////////// ///
      CDataTreatment::CDataTreatment (boost::shared_ptr<CContext> _ctxt)
         : currentContext(_ctxt), enabledFiles()
      { this->doTreatment(); }

      CDataTreatment::~CDataTreatment(void)
      { /* Ne rien faire de plus */ }

      ///---------------------------------------------------------------

      const boost::shared_ptr<CContext>
         CDataTreatment::getCurrentContext(void) const
      {
         return (this->currentContext);
      }

      const std::vector<boost::shared_ptr<CFile> > &
         CDataTreatment::getEnabledFiles (void) const
      {
         return (this->enabledFiles);
      }
      //----------------------------------------------------------------

      void CDataTreatment::update_calendar(int step)
      {
         // Mise à jour côté client
         this->currentContext->getCalendar()->update(step);
//         std::cout <<  "current date : " << this->currentContext->getCalendar()->getCurrentDate() << std::endl;
         if (CXIOSManager::GetStatus() == CXIOSManager::LOC_CLIENT)
         { // Mise à jour côté serveur
            boost::shared_ptr<comm::CClient> client = comm::CClient::GetClient();
            client->updateCalendar(step);
         }
      }

      void CDataTreatment::set_timestep(const date::CDuration & duration)
      {
         // Mise à jour côté client
         this->currentContext->getCalendar()->setTimeStep(duration);
         this->currentContext->timestep.setValue(duration.toString());
         if (CXIOSManager::GetStatus() == CXIOSManager::LOC_CLIENT)
         { // Mise à jour côté serveur
            boost::shared_ptr<comm::CClient> client = comm::CClient::GetClient();
            client->setTimestep(duration);
         }
      }

      void CDataTreatment::finalize(void)
      {
         // Mise à jour côté client
         if (CXIOSManager::GetStatus() == CXIOSManager::LOC_CLIENT)
         { // Mise à jour côté serveur
            boost::shared_ptr<comm::CClient> client = comm::CClient::GetClient();
            client->finalize();
         }
         else closeAllFile() ;
      }

      void CDataTreatment::closeAllFile(void )
      {
         std::vector<boost::shared_ptr<CFile> >::const_iterator
            it = this->enabledFiles.begin(), end = this->enabledFiles.end();
         
         for (; it != end; it++)
         {
            (*it)->close();
         }
      }
      //----------------------------------------------------------------
      
      void CDataTreatment::write_data
                           (const StdString & fieldId,
                            const StdString & fileId,
                            const std::deque<ARRAY(double, 1)> & data)
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
            
            if (file->getId().compare(fileId) == 0)
            {
               if (field->updateDataServer(currDate, data))
               {
                  file->getDataOutput()->writeFieldData(field);
               }
               return;
            }
         }

      }

      //----------------------------------------------------------------

      void CDataTreatment::doTreatment(void)
      {         
         // Résolution du calendrier
         this->currentContext->solveCalendar();         
         
         // Résolution des héritages pour le context actuel.
         //std::cout << "(Message temporaire) Résolution des héritages ..." << std::endl;
         this->solveAllInheritance();

         //Initialisation du vecteur 'enabledFiles' contenant la liste des fichiers à sortir.
         //std::cout << "(Message temporaire) Initialisation du vecteur enabledFiles ..." << std::endl;
         this->findEnabledFiles();

         //Recherche des champs à sortir (enable à true + niveau de sortie correct)
         // pour chaque fichier précédemment listé.
         //std::cout << "(Message temporaire) Recherche des champs à sortir ..." << std::endl;
         this->findAllEnabledFields();

         // Résolution des références de grilles pour chacun des champs.
         //std::cout << "(Message temporaire) Résolution des références de grilles ..." << std::endl;
         this->solveAllGridRef();

         // Traitement des opérations.
         this->solveAllOperation();

         // Nettoyage de l'arborescence
         CContext::CleanTree();

         //std::cout << "(Message temporaire) fin traitement sorties ..." << std::endl;
      }

      void CDataTreatment::findAllEnabledFields(void)
      {
         for (unsigned int i = 0; i < this->enabledFiles.size(); i++)
            (void)this->enabledFiles[i]->getEnabledFields();
      }

      void CDataTreatment::solveAllGridRef(void)
      {
         for (unsigned int i = 0; i < this->enabledFiles.size(); i++)
            this->enabledFiles[i]->solveEFGridRef();
      }

      void CDataTreatment::solveAllOperation(void)
      {
         for (unsigned int i = 0; i < this->enabledFiles.size(); i++)
            this->enabledFiles[i]->solveEFOperation();
      }

      void CDataTreatment::solveAllInheritance(void) const
      {
         // Résolution des héritages descendants (càd des héritages de groupes)
         // pour chacun des contextes.
         this->currentContext->solveDescInheritance();

         // Résolution des héritages par référence au niveau des fichiers.
          const std::vector<boost::shared_ptr<CFile> > & allFiles
            = CObjectFactory::GetObjectVector<CFile>();

         for (unsigned int i = 0; i < allFiles.size(); i++)
            allFiles[i]->solveFieldRefInheritance();
      }

      void CDataTreatment::findEnabledFiles(void)
      {
         const std::vector<boost::shared_ptr<CFile> > & allFiles
            = CObjectFactory::GetObjectVector<CFile>();

         for (unsigned int i = 0; i < allFiles.size(); i++)
            if (!allFiles[i]->enabled.isEmpty()) // Si l'attribut 'enabled' est défini.
               if (allFiles[i]->enabled.getValue()) // Si l'attribut 'enabled' est fixé à vrai.
                  enabledFiles.push_back(allFiles[i]);

         if (enabledFiles.size() == 0)
            DEBUG(<<"Aucun fichier ne va être sorti dans le contexte nommé \""
                  << this->currentContext->getId() << "\" !");
      }

      ///---------------------------------------------------------------

   } // namespace data
} // namespace xmlioserver

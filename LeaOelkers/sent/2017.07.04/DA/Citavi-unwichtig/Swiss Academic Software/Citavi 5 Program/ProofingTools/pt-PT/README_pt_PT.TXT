http://www.broffice.org/verortografico/baixar

Autor/Author: Raimundo Moura <raimundomoura@openoffice.org>

pt-BR: Este dicionário está em desenvolvimento por Raimundo Moura e sua equipe. Ele está
licenciado sob os termos da Licença Pública Geral Menor versão 2.1 (LGPLv2.1), como 
publicado pela Free Software Foundation. Os créditos estão disponíveis em 
http://www.broffice.org/creditos e você pode encontrar novas versões em 
http://www.broffice.org/verortografico.

en-US: This dictionary is under development by Raimundo Moura and his team. It is 
licensed under the terms of the GNU Lesser General Public License version 2.1(LGPLv2.1),
as published by the Free Software Foundation. The credits are available at 
http://www.broffice.org/creditos and you can find new releases at 
http://www.broffice.org/verortografico.


Copyright (C) 2006 - 2009 por/by Raimundo Santos Moura <raimundomoura@openoffice.org>

=============
APRESENTAÇÃO
=============

O Projeto Verificador Ortográfico do BrOffice.org é um projeto 
colaborativo desenvolvido pela comunidade Brasileira. 
A relação completa dos colaboradores deste projeto está em: 
http://www.openoffice.org.br/?q=creditos

***********************************************************************
* Este é um dicionário para correção ortográfica da língua Portuguesa *
* para o Myspell.                                                     *
* Este programa é livre e pode ser redistribuído e/ou modificado nos  *
* termos da GNU Lesser General Public License (LGPL) versão 2.1.      *
*                                                                     *
***********************************************************************

=======================
SOBRE ESTA ATUALIZAÇÃO
=======================
. Alteração da regra 'B' para inclusão de plural para sufixos 'és'
  Exemplo: viés - vieses;
. Correção da regra 'a' para conjugação correta do verbo 'cuspir'.
  Não estava contemplado 'cospem';
. Correção da regra 'm' para verbos terminados em quir/guir nas formas de
  ênclises e mesóclises
. Inclusão do comando BREAK para permitir a verificação correta dos compostos;
. Inclusão do comando MAXNGRAMSUGS para limitar o número de sugestões;
. Atualização da regra REP melhorando as opções de sugestão;
. Inclusão de 'hidrossanitário', colaboração de Gilmar Grespan;
. Inclusão de 'tropeirismo', colaboração de Tiago Hillebrandt;
. Correção da regra 'a' para verbos para inclusão de superlativos;
. Inclusão de: marquetagem, lasqueira, esquerdopata, latinoide, colaboração Edson Costa;
. Correção da regra de sugestões 'REP';
. Inclusão de: sudoestino e Sanepar, colaboração Marcos Vinícius Piccinini   
. Correção da conjugação de verbos nas formas de ênclises e mesóclises;
. Inclusão de regras para composição de paises;
. Inclusão das siglas dos partidos políticos brasileiros;
. reforço do prefixo ex-; 
. Inclusão de: precursoramente, neoconstitucionalismo, alopoiese, alopoiético,
  autopoiético, BrOffice, paradoxização, programaticidade, sistemismo, comteano,
  durkheimiano e luhmanniano. Colaboração Pablo Feitosa;
. Exclusão de 'fundamenais'. Colaboração de João Paulo Vinha Bittar;
  Judicialização.  Colaboração Pablo Feitosa;
. Inclusão de: Judicialização, externaliza, externalizado, externalização, externalizei, etc.
  Paradoxização, desparadoxização, procedimentalizar, procedimentalização , 
  procedimentalizo, etc. Colaboração Pablo Feitosa;
. Alterações no Divsilab. Colaboração Flavio Figueiredo Cardoso;
. Aplicação das regras de prefíxo: sub, super, auto, re, inter, etc. na formação de novos
  compostos.  
. Inclusão de:  racistoide, melequento, burraldo, entregação, prum, pruns
  (contração para+artigo), comezinha, yakuzá (máfia japonesa) e figuraça. 
  Exclusão de: 'pãozinhos' no plural está incorreto. Colaboração de Edson Costa.
. Inclusão de 'dimetilsulfato' e 'dietilsulfato'. Colaboração Luis Alcides Brandini De Boni;
===========================================================
INSTRUÇÕES PARA INSTALAÇÃO E CONFIGURAÇÃO DO DICIONÁRIO
===========================================================

Copie os ficheiros pt_PT.dic e pt_PT .aff para a pasta (diretório)
 <OpenOffice.org>/share/dict/ooo, onde <OpenOffice.org> é o local em 
  que o programa foi instalado.
No Windows, normalmente, o caminho é este: 
C:\Program Files\OpenOffice.org 2.0\share\dict\ooo, 
e no  Linux  /opt/OpenOffice.org/share/dict/ooo/.

Na mesma pasta (diretório), localize o ficheiro dictionary.lst. 
Abra-o com um editor de textos e acrescente a seguinte linha ao final
(caso ainda não exista):

DICT pt PT pt_PT

É necessário reiniciar o OpenOffice, inclusive o 'início rápido' 
da versão para Windows que fica na barra de tarefas, para que o 
corretor funcione.

=======================================================
DÚVIDAS FREQUENTES
=======================================================

Os ficheiros foram copiados mas o Corretor não está funcionando.
O Corretor Ortográfico não deve estar configurado corretamente, isto
pode estar ocorrendo por um dos seguintes motivos:

1- O dicionário provavelmente não está instalado.

Para se certificar de que está utilizando o idioma correto confira como
estão as informações em: Ferramentas >> Opções >>   Configurações de
Idioma >> Idiomas. O item Ocidental deve apresentar o dicionário
selecionado (deve aparecer um logo "Abc" do lado do idioma).

Se não estiver Português (Brasil) mude para esse idioma. Após
configurado clique em 'OK'.
Feche o BrOffice, inclusive o Iniciador Rápido,  e em seguida reabra-o;


2 - O verificador não está configurado para verificar texto ao digitar.
Neste caso confira como estão as informações em: Ferramentas >> Opções
>> Configurações de Idiomas >> Recursos de Verificação Ortográfica e, 
no campo opções deste formulário marque a opção 'Verificar texto ao 
digitar';


Novas atualizações estarão disponíveis no site do BrOffice.Org, na
página do Verificador Ortográfico.

http://www.openoffice.org.br/?q=verortografico
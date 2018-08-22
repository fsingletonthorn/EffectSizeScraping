from itertools import chain
from pubmed_parser.utils import read_xml, stringify_children, month_or_day_formater

__all__ = [
    'parse_medline_xml',
    'parse_medline_grant_id'
]

def parse_pmid(medline):
    """Parse PMID from article

    Parameters
    ----------
    medline: Element
        The lxml node pointing to a medline document

    Returns
    -------
    pmid: str
        String version of the PubMed ID
    """
    if medline.find('PMID') is not None:
        pmid = medline.find('PMID').text
    else:
        pmid = ''
    return pmid


def parse_mesh_terms(medline):
    """Parse MESH terms from article

    Parameters
    ----------
    medline: Element
        The lxml node pointing to a medline document

    Returns
    -------
    mesh_terms: str
        String of semi-colon spearated MeSH (Medical Subject Headings)
        terms contained in the document.
    """
    if medline.find('MeshHeadingList') is not None:
        mesh = medline.find('MeshHeadingList')
        mesh_terms_list = [m.find('DescriptorName').text for m in mesh.getchildren()]
        mesh_terms = '; '.join(mesh_terms_list)
    else:
        mesh_terms = ''
    return mesh_terms


def parse_keywords(medline):
    """Parse keywords from article, separated by ;

    Parameters
    ----------
    medline: Element
        The lxml node pointing to a medline document

    Returns
    -------
    keywords: str
        String of concatenated keywords.
    """
    keyword_list = medline.find('KeywordList')
    keywords = list()
    if keyword_list is not None:
        for k in keyword_list.findall('Keyword'):
            keywords.append(k.text)
        keywords = '; '.join(keywords)
    else:
        keywords = ''
    return keywords


def parse_other_id(medline):
    """Parse OtherID from article, each separated by ;

    Parameters
    ----------
    medline: Element
        The lxml node pointing to a medline document

    Returns
    -------
    other_id: str
        String of semi-colon separated Other IDs found in the document
    """
    pmc = ''
    other_id = list()
    oids = medline.findall('OtherID')
    if oids is not None:
        for oid in oids:
            if 'PMC' in oid.text:
                pmc = oid.text
            else:
                other_id.append(oid.text)
        other_id = '; '.join(other_id)
    else:
        other_id = ''
    return {'pmc': pmc,
            'other_id': other_id}


def parse_grant_id(medline):
    """Parse Grant ID and related information given MEDLINE tree

    Parameters
    ----------
    medline: Element
        The lxml node pointing to a medline document

    Returns
    -------
    grant_list: list
        List of grants acknowledged in the publications. Each
        entry in the dictionary contains the PubMed ID,
        grant ID, grant acronym, country, and agency.
    """
    article = medline.find('Article')
    pmid = parse_pmid(medline)

    grants = article.find('GrantList')
    grant_list = list()
    if grants is not None:
        grants_list = grants.getchildren()
        for grant in grants_list:
            grant_country = grant.find('Country')
            if grant_country is not None:
                country = grant_country.text
            else:
                country = ''
            grant_agency = grant.find('Agency')
            if grant_agency is not None:
                agency = grant_agency.text
            else:
                agency = ''
            grant_acronym = grant.find('Acronym')
            if grant_acronym is not None:
                acronym = grant_acronym.text
            else:
                acronym = ''
            grant_id = grant.find('GrantID')
            if grant_id is not None:
                gid = grant_id.text
            else:
                gid = ''
            dict_ = {'pmid': pmid,
                     'grant_id': gid,
                     'grant_acronym': acronym,
                     'country': country,
                     'agency': agency}
            grant_list.append(dict_)
    return grant_list


def date_extractor(journal, year_info_only):
    """Extract PubDate information from an Article in the Medline dataset.

    Parameters
    ----------
    journal: Element
        The 'Journal' field in the Medline dataset
    year_info_only: bool
        if True, this tool will only attempt to extract year information from PubDate.
        if False, an attempt will be made to harvest all available PubDate information.
        If only year and month information is available, this will yield a date of
        the form 'YYYY-MM'. If year, month and day information is available,
        a date of the form 'YYYY-MM-DD' will be returned.

    Returns
    -------
    PubDate: str
        PubDate extracted from an article.
        Note: If year_info_only is False and a month could not be
        extracted this falls back to year automatically.
    """
    day = None
    month = None
    issue = journal.xpath('JournalIssue')[0]
    issue_date = issue.find('PubDate')

    if issue_date.find('Year') is not None:
        year = issue_date.find('Year').text
        if not year_info_only:
            if issue_date.find('Month') is not None:
                month = month_or_day_formater(issue_date.find('Month').text)
                if issue_date.find('Day') is not None:
                    day = month_or_day_formater(issue_date.find('Day').text)
    elif issue_date.find('MedlineDate') is not None:
        year_text = issue_date.find('MedlineDate').text
        year = year_text.split(' ')[0]
    else:
        year = ""

    if year_info_only or month is None:
        return year
    else:
        return "-".join(str(x) for x in filter(None, [year, month, day]))


def parse_article_info(medline, year_info_only):
    """Parse article nodes from Medline dataset

    Parameters
    ----------
    medline: Element
        The lxml node pointing to a medline document
    year_info_only: bool
        see: date_extractor().

    Returns
    -------
    article: dict
        Dictionary containing information about the article, including
        `title`, `abstract`, `journal`, `author`, `affiliation`, `pubdate`,
        `pmid`, `other_id`, `mesh_terms`, and `keywords`. The field
        `delete` is always `False` because this function parses
        articles that by definition are not deleted.
    """
    article = medline.find('Article')

    if article.find('ArticleTitle') is not None:
        title = stringify_children(article.find('ArticleTitle'))
    else:
        title = ''

    if article.find('Abstract') is not None:
        abstract = stringify_children(article.find('Abstract'))
    else:
        abstract = ''

    if article.find('AuthorList') is not None:
        authors = article.find('AuthorList').getchildren()
        authors_info = list()
        affiliations_info = list()
        for author in authors:
            if author.find('Initials') is not None:
                firstname = author.find('Initials').text
            else:
                firstname = ''
            if author.find('LastName') is not None:
                lastname = author.find('LastName').text
            else:
                lastname = ''
            if author.find('AffiliationInfo/Affiliation') is not None:
                affiliation = author.find('AffiliationInfo/Affiliation').text
            else:
                affiliation = ''
            authors_info.append(firstname + ' ' + lastname)
            affiliations_info.append(affiliation)
        affiliations_info = ' '.join([a for a in affiliations_info if a is not ''])
        authors_info = '; '.join(authors_info)
    else:
        affiliations_info = ''
        authors_info = ''

    journal = article.find('Journal')
    journal_name = ' '.join(journal.xpath('Title/text()'))
    pubdate = date_extractor(journal, year_info_only)

    pmid = parse_pmid(medline)
    mesh_terms = parse_mesh_terms(medline)
    keywords = parse_keywords(medline)
    other_id_dict = parse_other_id(medline)

    dict_out = {'title': title,
                'abstract': abstract,
                'journal': journal_name,
                'author': authors_info,
                'affiliation': affiliations_info,
                'pubdate': pubdate,
                'pmid': pmid,
                'mesh_terms': mesh_terms,
                'keywords': keywords,
                'delete': False}
    dict_out.update(other_id_dict)
    return dict_out


def parse_medline_xml(path, year_info_only=True):
    """Parse XML file from Medline XML format available at
    ftp://ftp.nlm.nih.gov/nlmdata/.medleasebaseline/gz/

    Parameters
    ----------
    path: str
        The path
    year_info_only: bool
        if True, this tool will only attempt to extract year information from PubDate.
        if False, an attempt will be made to harvest all available PubDate information.
        If only year and month information is available, this will yield a date of
        the form 'YYYY-MM'. If year, month and day information is available,
        a date of the form 'YYYY-MM-DD' will be returned.
        NOTE: the resolution of PubDate information in the Medline(R) database varies
        between articles.
        Defaults to True.

    Returns
    -------
    article_list: list
        Dictionary containing information about articles in NLM format (see
        `parse_article_info`). Articles that have been deleted will be
        added with no information other than the field `delete` being `True`
    """
    tree = read_xml(path)
    medline_citations = tree.xpath('//MedlineCitationSet/MedlineCitation')
    article_list = list(map(lambda m: parse_article_info(m, year_info_only), medline_citations))
    delete_citations = tree.xpath('//DeleteCitation/PMID')
    dict_delete = \
        [
            {'title': None,
             'abstract': None,
             'journal': None,
             'author': None,
             'affiliation': None,
             'pubdate': None,
             'pmid': p.text,
             'other_id': None,
             'mesh_terms': None,
             'keywords': None,
             'delete': True
             } for p in delete_citations
            ]
    article_list.extend(dict_delete)
    return article_list


def parse_medline_grant_id(path):
    """Parse grant id from Medline XML file

    Parameters
    ----------
    path: str
        The path to the XML with the information

    Returns
    -------
    grant_id_list: list
        List of dictionaries for all files in `path`. Each dictionary
        will have the information returned by `parse_grant_id`
    """
    tree = read_xml(path)
    medline_citations = tree.xpath('//MedlineCitationSet/MedlineCitation')
    grant_id_list = list(map(parse_grant_id, medline_citations))
    grant_id_list = list(chain(*grant_id_list)) # flatten list
    return grant_id_list

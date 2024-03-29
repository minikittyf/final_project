% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newyorktimes.R
\name{rss_rank}
\alias{rss_rank}
\title{Section Rank.}
\usage{
rss_rank(search_section = "Arts")
}
\arguments{
\item{search_section}{A Character.(the character should choose from: Africa, Americas, ArtandDesign, Arts, AsiaPacific, Automobile, Baseball, Books, Business, Climate, CollegeBasketball, CollegeFootball, Dance, Dealbook, DiningandWine, Economy, Education, EnergyEnvironment, Europe, FashionandStyle, Golf, Health, Hockey, HomePage, Jobs, Lens, MediaandAdvertising, MiddleEast, MostEmailed, MostShared, MostViewed, Movies, Music, NYRegion, Obituaries, PersonalTech, Politics, ProBasketball, ProFootball, RealEstate, Science, SmallBusiness, Soccer, Space, Sports, SundayBookReview, Sunday-Review, Technology, Television, Tennis, Theater, TMagazine, Travel, Upshot, US, Weddings, Well, YourMoney).}
}
\value{
A dataframe inclde article's title, link, description, published date, and their rank based on \code{search_section}.
}
\description{
Get recent ranked articles' information in specific section.
}
\examples{
rss_rank('Arts')

}

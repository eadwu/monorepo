/* @flow */
import HTTPS from 'HTTPS'
import { loop } from 'helpers'

(() => {
  const currDate = new Date()
  const currMonth: number = currDate.getMonth() + 1
  const currDay: number = currDate.getDate()
  const currYear: number = currDate.getFullYear()
  const formattedMonth: string = currMonth < 10 ? `0${currMonth}` : currMonth.toString()
  const currFormat: string = `${currYear}-${formattedMonth}-${currDay}`

  let tick: number = -1

  document.addEventListener('DOMContentLoaded', () => {
    const year: HTMLDivElement = (document.querySelector('#currYear'): any)
    const date: HTMLDivElement = (document.querySelector('#currDate'): any)
    const content: HTMLDivElement = (document.querySelector('.content'): any)

    function newChild (air: string, title: string, parent?: any) {
      ReactDOM.render((
        <div id='nekopara'>
          <div className='air-time z-depth-1 cell grey white-text'>
            <span className='time white-text'>{air}</span>
          </div>
          <div className='anime-title z-depth-1 grey darken-1'>
            <span className='title white-text'>{title.toUpperCase()}</span>
          </div>
        </div>
      ), parent || content)
    }

    function refresh (timestamp: number) {
      if (timestamp - tick >= 10800 || tick < 0) {
        tick = timestamp
        HTTPS.POST('anilist.co/api/auth/access_token', {
          grant_type: 'client_credentials',
          client_id: 'karappo-sblsi',
          client_secret: 'K1GlIcLxBier3jNQQbNvlNu4'
        }, {
          'Content-Type': 'application/x-www-form-urlencoded'
        })
          .then((raw) => {
            const response = JSON.parse(raw)
            const token: string = response.access_token
            HTTPS.GET('anilist.co/api/browse/anime', {
              year: 2017,
              season: 'spring',
              status: 'currently airing',
              full_page: true,
              airing_data: true,
              sort: 'start_date',
              access_token: token
            }, {
              'Content-Type': 'application/x-www-form-urlencoded'
            })
              .then((rawResponse) => {
                const res = JSON.parse(rawResponse)
                loop(res, 0, (anime) => {
                  const title: string = anime.title_english
                  const titleRomanji: string = anime.title_romaji
                  if (anime.airing) {
                    const airTime: string = anime.airing.time
                    const extractedDate = airTime.match(/([\d-]+)T/)
                    const extractedTime = airTime.match(/T([\d:]+)\+/)
                    const augmentedDate: string = extractedDate ? extractedDate[1] : ''
                    const augmentedTime: string = extractedTime ? extractedTime[1].substr(0, 5) : ''
                    if (augmentedDate === currFormat) { newChild(augmentedTime, title) }
                  }
                })
              })
          })
      }
      requestAnimationFrame(refresh)
    }

    year.textContent = currYear.toString()
    date.textContent = `${currMonth}-${currDay}`
    requestAnimationFrame(refresh)
  })
}).call(this)

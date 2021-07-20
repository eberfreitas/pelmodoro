# Pelmodoro

[**Pelmodoro**](https://www.pelmodoro.com/) is an attempt to create a somewhat feature-complete Pomodoro timer app so you can track your working sessions using the Pomodoro technique.

It runs in the browser but can also be installed as a stand-alone app as a PWA on mobile devices or other browsers that allow that type of installation like Edge.

## What is the Pomodoro technique?

> The Pomodoro Technique is a time management method developed by Francesco Cirillo in the late 1980s. The technique uses a timer to break down work into intervals, traditionally 25 minutes in length, separated by short breaks. Each interval is known as a pomodoro, from the Italian word for 'tomato', after the tomato-shaped kitchen timer that Cirillo used as a university student.
> -- Pomodoro Technique from Wikipedia

## Features

- Personalize your sessions
- Productivity / sentiment logs
- Stats
- Multiple means of notification like sounds and browser notifications
- Spotify integration to sync a playlist to a working session
- Multiple color themes
- Export your stats data
- And more...

## What about the name?

Pelmodoro started as a side-project to improve my skills in writing [Elm](https://elm-lang.org/), hence the name p**ELM**odoro. I'm not a huge fan of using tech names on product names, but in this case, it just felt right.

## Contributing

Like said before, Pelmodoro is written almost entirely in the Elm language. If you are willing to contribute with code, you might wanna take a look at the language's [guide](https://guide.elm-lang.org/) if you are not familiar with it yet. It is easy and fun, **you should give it a try**!

For the most part, I consider this project done, but there are few areas where contributions are much appreciated:

**Outstanding bugs**: If you find any bugs that prevent you from using the app, create an issue describing the problem and we can work it out.

**New themes**: If you have any ideas for a new theme or just want to import a different color scheme to Pelmodoro, take a look at how themes are implemented [here](https://github.com/eberfreitas/pelmodoro/blob/main/src/Themes/Theme.elm) and [here](https://github.com/eberfreitas/pelmodoro/blob/main/src/Themes/Tomato.elm). New themes are always appreciated.

**Quotes**: You can contribute with quotes related to productivity, mindfulness, awareness, and so on. Take a look at the quotes we already have [here](https://github.com/eberfreitas/pelmodoro/blob/main/src/Quotes.elm) to get a sense of the type of quotes we use on the app. Try to bring insightful ideas that can aggregate to everyday real life.

**Alarm sounds**: We can always go with more options. If you find a cool sound, we can add it. Just make sure that we can use it freely. [Freesound](https://freesound.org/) seems to be a good place to find new sounds.

**Spelling and grammatical errors**: If you spot any words or expressions that are wrong or can be improved, just let me know or send your PR. There are probably a bunch of those as english is not my first language.

**PWA improvements**: This app is my first attempt at creating a PWA application. I'm sure there are ways to improve things there.

Other ideas might be discussed on issues, just let me know.

## Running locally

Clone the repo and run the following commands:

```
$ cp .env.sample .env
$ npm install
$ npm run dev
```

Now go to `https://localhost:1234` and you should see the app running.

You can also build the project running:

```
$ npm run build
```

Built artifacts will be available in the `/dist` folder.

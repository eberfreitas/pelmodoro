# Pelmodoro

[**Pelmodoro**](https://www.pelmodoro.com/) is an attempt to create a somewhat feature-complete Pomodoro timer app so you can track your working sections using the Pomodoro technique.

It runs in the browser but can also be installed as a stand-alone app as a PWA on mobile devices or other browsers that allow that type of installation like Edge.

## What is the Pomodoro technique?

> The Pomodoro Technique is a time management method developed by Francesco Cirillo in the late 1980s. The technique uses a timer to break down work into intervals, traditionally 25 minutes in length, separated by short breaks. Each interval is known as a pomodoro, from the Italian word for 'tomato', after the tomato-shaped kitchen timer that Cirillo used as a university student.
> -- Pomodoro Technique from Wikipedia

## Features

- Personalize your sections your way
- Stats
- Multiple means of notification like sounds and browser notifications
- Spotify integration to sync a playlist to a working section
- Multiple color schemes/themes
- Export your stats data
- And more...

## What about the name?

Pelmodoro started as a side-project to improve my skills in writing [Elm](https://elm-lang.org/), hence the name P**ELM**odoro.

## Contributing

Like said before, Pelmodoro is written almost entirely in the Elm language. If you are willing to contribute with code, you might wanna take a look at the language's [guide](https://guide.elm-lang.org/).

For the most part, I consider this project done, so there are only a few areas where contributions would be appreciated:

**Outstanding bugs**: If you find any bugs that prevent you from using the app, create an issue describing the problem and we can work it out.

**New themes**: If you have any ideas for a new theme or just want to adapt a different color scheme to Pelmodoro, take a look at how themes are implemented [here](https://github.com/eberfreitas/pelmodoro/blob/main/src/Themes/Theme.elm) and [here](https://github.com/eberfreitas/pelmodoro/blob/main/src/Themes/Tomato.elm). New themes are always appreciated.

Other ideas might be discussed on issues, just let me know.

## Running locally

Just clone the repo and run the following commands:

```
$ npm install
$ npm run dev
```

You can also build running:

```
$ npm run build
```

Built artifacts will be available in the `/dist` folder.
